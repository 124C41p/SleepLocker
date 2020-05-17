import express, { Express } from 'express';
import { characterClasses, uniqueLoot } from './configurations';
// import { allLoot } from './configurations';
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
            let renderData = {
                raidName: raid.name,
                date: raid.date
            };
            let flags = {
                classes: characterClasses,
                loot: uniqueLoot.filter(dungeon => dungeon.name == raid.dungeon)[0],
            }
            return res.render('register', { ...renderData, flags: JSON.stringify(flags) });
        case 2:
            return res.render('table', {
                locks: await db.listRaidLocks(raidID),
                raidName: raid.name,
                date: raid.date
            });
        default:
            res.sendStatus(500);
    }
});

createHttpServer(app, 12345);