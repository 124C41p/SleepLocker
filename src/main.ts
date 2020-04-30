import express, { Express } from 'express';
import { RaidDatabase, SoftlockCapacityReachedError, SoftlockRegistrationError, SoftlockCancellationError } from './database';
import http from 'http';
import session from 'express-session';
import flash from 'express-flash';

const app = express();
app.set('views', 'views');
app.set('view engine', 'pug');
app.use(express.urlencoded({ extended: true }));
app.use(session({secret: 'sleeplocker', resave: false, saveUninitialized: false, cookie: { secure: false, maxAge: 86400000 }}))
app.use(flash());

function createHttpServer(app: Express, port: number = 8080) {
    http.createServer(app).listen(port, () => console.log(`http server is running on port ${port}`));
}

let db = new RaidDatabase();
db.initialize();

app.get('/', async (req, res) => {
    let raidID = await db.current_raid_id();
    if(!raidID) return res.render('index');
    let raid = await db.get_raid(raidID);

    switch(raid.mode) {
        case 0:
            return res.render('almostReady', {
                raidName: raid.name,
                date: raid.date
            });
        case 1:
            let renderData = {
                name: '',
                prio1: '',
                prio2: '',
                allowChanges: true,
                raidName: raid.name,
                date: raid.date
            };
            let name = (req.session as Express.Session).name as string|null;
            if(name) {
                renderData.name = name;
                let locks = await db.get_user_locks(raidID, name);
                if(locks) {
                    renderData.prio1 = locks.prio1;
                    renderData.prio2 = locks.prio2;
                    renderData.allowChanges = false;
                }
            }
            return res.render('register', renderData);
        case 2:
            return res.render('table', {
                locks: await db.list_raid_locks(raidID),
                raidName: raid.name,
                date: raid.date
            });
        default:
            res.sendStatus(500);
    }
});

app.post('/register_lock', async (req,res) => {
    try {
        let raidID = await db.current_raid_id();
        if(!raidID)
            throw new Error('Es gibt zur Zeit keinen aktiven Raid.');
        let raid = await db.get_raid(raidID);
        if(raid.mode != 1)
            throw new Error('Softlocking ist nicht mehr möglich.');
        if('register' in req.body) {
            let name = req.body.name as string
            if(name.length == 0)
                throw new Error('Ungültiger Name');
            await db.set_user_locks(raidID, name, req.body.prio1, req.body.prio2);
            (req.session as Express.Session).name = name;
            req.flash('successMsg', 'Softlocks wurden angenommen.')

        } else if ('delete' in req.body) {
            let name = (req.session as Express.Session).name as string;
            if(!name)
                throw new Error('Zur aktuellen Sitzung ist kein Charaktername bekannt.');
            await db.remove_lock(raidID, name);
        }
    } catch(err) {
        if(err instanceof SoftlockCapacityReachedError)
            req.flash('errorMsg', 'Die maximale Anzahl an Softlocks wurde erreicht.');
        else if(err instanceof SoftlockRegistrationError)
            req.flash('errorMsg', 'Softlocks konnten nicht angenommen werden. Bitte wende dich an die Raidleitung.');
        else if(err instanceof SoftlockCancellationError)
            req.flash('errorMsg', 'Softlocks konnten nicht storniert werden. Bitte wende dich an die Raidleitung.');
        else
            req.flash('errorMsg', 'Interner Serverfehler.');
    } finally {
        res.redirect('back');
    }
});

createHttpServer(app, 12345);