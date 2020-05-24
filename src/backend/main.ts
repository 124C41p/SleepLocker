import express, { Express } from 'express';
import { characterClasses, getLootLocations, getLootTable } from './configurations';
import apiRouter from './api';
import http from 'http';
import { initialize, getRaid, getUserList } from './database';

const app = express();
app.set('views', 'views');
app.set('view engine', 'pug');
app.use('/public/javascripts/', express.static('dist/frontend/'));
app.use('/api', apiRouter);

async function createHttpServer(app: Express, port: number = 8080) {
    await initialize()
    http.createServer(app).listen(port, () => console.log(`http server is running on port ${port}`));
}

app.get('/', async (req, res) => {
    res.render('index');
});

app.get('/:key', async (req, res, next) => {
    let key = req.params.key as string;
    let raid = await getRaid(key);
    if(raid == null)
        return next();

    if(key.length == 20) {
        return res.render('admin', {
            raidName: raid.title,
            date: raid.createdOn,
            flags: JSON.stringify({
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
                }
                return res.render('register', {
                    raidName: raid.title,
                    date: raid.createdOn,
                    flags: JSON.stringify(registerFlags)
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
                    date: raid.createdOn,
                    flags: JSON.stringify(tableFlags)
                });
        }
    }

    next();
});

app.get('*', (req, res) => {
    res.render('error');
});

createHttpServer(app, 12345);