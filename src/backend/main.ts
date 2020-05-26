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

function createFlags(obj: any) {
    let jsonStr = JSON.stringify(obj);
    jsonStr = jsonStr.split('\\').join('\\\\');
    jsonStr = jsonStr.split('\'').join('\\\'');
    return jsonStr;
}

app.get('/', async (req, res) => {
    res.render('index', {
        flags: createFlags(getDungeons())
    });
});

app.get('/:key', async (req, res, next) => {
    let key = req.params.key as string;
    let raid = await getRaid(key);
    if(raid == null)
        return next();

    if(key.length == 20) {
        return res.render('admin', {
            raidName: raid.title,
            date: raid.createdOn.toLocaleDateString(),
            flags: createFlags({
                adminKey: key,
                userKey: raid.userKey
            })
        });
    } else if(key.length == 6) {
        switch(raid.mode) {
            case 0:
                let registerFlags = {
                    classDescriptions: characterClasses,
                    lootTable: raid.dungeonKey == null ? null : getLootLocations(raid.dungeonKey),
                    raidID: key,
                    comments: raid.comments
                }
                return res.render('register', {
                    raidName: raid.title,
                    date: raid.createdOn.toLocaleDateString(),
                    flags: createFlags(registerFlags)
                });
            case 1:
                let lootTable = raid.dungeonKey == null ? null : getLootTable(raid.dungeonKey);
                let tableFlags = {
                    userList: await getUserList(key),
                    lootInformation: lootTable == null ? null : {
                        locations: lootTable.locations,
                        loot: lootTable.loot
                    }
                }
                return res.render('tables', {
                    raidName: raid.title,
                    date: raid.createdOn.toLocaleDateString(),
                    flags: createFlags(tableFlags)
                });
        }
    }

    next();
});

app.get('*', (req, res) => {
    res.render('error');
});

createHttpServer(app, 12345);