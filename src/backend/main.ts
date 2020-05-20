import express, { Express } from 'express';
import { characterClasses, getLootLocations, getLootTable } from './configurations';
import apiRouter from './api';
import http from 'http';
import { RaidDatabase } from './database';

const app = express();
app.set('views', 'views');
app.set('view engine', 'pug');
app.use('/public/javascripts/', express.static('dist/frontend/'));
app.use('/api', apiRouter);

let db = new RaidDatabase();

async function createHttpServer(app: Express, port: number = 8080) {
    await db.initialize()
    http.createServer(app).listen(port, () => console.log(`http server is running on port ${port}`));
}

app.get('/', async (req, res) => {
    let raidID = await db.currentRaidId();
    if(!raidID) return res.render('index');
    let raid = await db.getRaid(raidID);

    switch(raid.mode) {
        case 0:
            return res.render('almostReady', {
                raidName: raid.name,
                date: raid.date
            });
        case 1:
            let registerFlags = {
                classDescriptions: characterClasses,
                lootTable: getLootLocations(raid.dungeon),
            }
            return res.render('register', {
                raidName: raid.name,
                date: raid.date,
                flags: JSON.stringify(registerFlags)
            });
        case 2:
            let lootTable = getLootTable(raid.dungeon)
            let tableFlags = {
                userList: await db.listRaidLocks(raidID),
                lootInformation: lootTable == null ? null : {
                    locations: lootTable.locations,
                    loot: lootTable.loot
                }
            }
            return res.render('tables', {
                raidName: raid.name,
                date: raid.date,
                flags: JSON.stringify(tableFlags)
            });
        default:
            res.sendStatus(500);
    }
});

app.get('/provisional_create_raid/:key/:name', async (req, res) => {
    let name = req.params.name as string;
    let key = req.params.key as string;
    console.log(name, key);
    let adminKey = await db.createRaid(name, key);
    res.redirect('/' + adminKey)
});

app.get('/:key', async (req, res, next) => {
    let key = req.params.key as string;
    if(key.length != 20)
        return next();
    let raid = await db.getRaidByAdminKey(key);
    if(raid == null)
        return next();

    res.render('admin', {
        raidName: raid.name,
        date: raid.date,
        flags: JSON.stringify(key)
    });
});

createHttpServer(app, 12345);