import express, { Express } from 'express';
import { characterClasses, getLootLocations, getLootTable, getDungeons } from './configurations';
import apiRouter from './api';
import http from 'http';
import { initialize, getRaid, getUserList } from './database';

const app = express();
app.set('views', 'views');
app.set('view engine', 'pug');
app.use('/public/javascripts/', express.static('dist/frontend/'));
app.use('/api', apiRouter);

async function createHttpServer(app: Express, port: number = 8080) {
    try {
        await initialize();
        http.createServer(app).listen(port, () => console.log(`http server is running on port ${port}`));
    } catch(err) {
        console.error(err);
    }
}

app.get('*', (req, res, next) => {
    let agent = req.headers['user-agent'];
    if(agent != null && /MSIE|Trident|Edge/.test(agent))
        return res.render('ie');
    next();
});

app.get('/', (req, res) => {
    res.render('index', {
        flags: getDungeons()
    });
});

app.get('/:key', async (req, res, next) => {
    let key = req.params.key as string;
    let raid = await getRaid(key);
    if(raid == null)
        return next();

    if(key.length == 20) {
        return res.render('admin', {
            flags: {
                raidAdminKey: key,
                raidUserKey: raid.raidUserKey,
                title: raid.title,
                createdOn: raid.createdOn.toISOString()
            }
        });
    } else if(key.length == 6) {
        switch(raid.mode) {
            case 0:
                let registerFlags = {
                    classDescriptions: characterClasses,
                    lootTable: raid.dungeonKey == null ? null : getLootLocations(raid.dungeonKey),
                    raidID: key,
                    comments: raid.comments,
                    title: raid.title,
                    createdOn: raid.createdOn.toISOString()
                }
                return res.render('register', {
                    flags: registerFlags
                });
            case 1:
                let lootTable = raid.dungeonKey == null ? null : getLootTable(raid.dungeonKey);
                let tableFlags = {
                    userList: await getUserList(key),
                    lootInformation: lootTable == null ? null : {
                        locations: lootTable.locations,
                        loot: lootTable.loot
                    },
                    title: raid.title,
                    createdOn: raid.createdOn.toISOString()
                }
                return res.render('tables', {
                    flags: tableFlags
                });
        }
    }

    next();
});

app.get('*', (req, res) => {
    res.render('error');
});

createHttpServer(app, 12345);