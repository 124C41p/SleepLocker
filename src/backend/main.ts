import express, { Express } from 'express';
import { RaidDatabase, UserData } from './database';
import { characterClasses, uniqueLoot } from './configurations';
import http from 'http';
import session from 'express-session';
import flash from 'express-flash';

const app = express();
app.set('views', 'views');
app.set('view engine', 'pug');
app.use(express.json());
app.use(session({secret: 'sleeplocker', resave: false, saveUninitialized: false, cookie: { secure: false, maxAge: 86400000 }}))
app.use('/public/javascripts/', express.static('dist/frontend/'));
app.use(flash());

function createHttpServer(app: Express, port: number = 8080) {
    http.createServer(app).listen(port, () => console.log(`http server is running on port ${port}`));
}

let db = new RaidDatabase();
db.initialize();


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
                loot: uniqueLoot.filter(instance => instance.name == raid.instance)[0],
                userData: null as UserData|null
            }
            let name = (req.session as Express.Session).name as string|null;
            if(name) {
                let data = await db.getUserData(raidID, name);
                if(data)
                    flags.userData = data;
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

// app.post('/register_lock', async (req,res) => {
//     try {
//         let raidID = await db.currentRaidId();
//         if(!raidID)
//             throw new Error('Es gibt zur Zeit keinen aktiven Raid.');
//         let raid = await db.getRaid(raidID);
//         if(raid.mode != 1)
//             throw new Error('Softlocking ist nicht mehr möglich.');
//         if('register' in req.body) {
//             let name = req.body.name as string
//             if(name.length == 0)
//                 throw new Error('Ungültiger Name');
//             await db.setUserLocks(raidID, name, req.body.prio1, req.body.prio2);
//             (req.session as Express.Session).name = name;
//             req.flash('successMsg', 'Softlocks wurden angenommen.')

//         } else if ('delete' in req.body) {
//             let name = (req.session as Express.Session).name as string;
//             if(!name)
//                 throw new Error('Zur aktuellen Sitzung ist kein Charaktername bekannt.');
//             await db.remove_lock(raidID, name);
//         }
//     } catch(err) {
//         if(err instanceof SoftlockCapacityReachedError)
//             req.flash('errorMsg', 'Die maximale Anzahl an Softlocks wurde erreicht.');
//         else if(err instanceof SoftlockRegistrationError)
//             req.flash('errorMsg', 'Softlocks konnten nicht angenommen werden. Bitte wende dich an die Raidleitung.');
//         else if(err instanceof SoftlockCancellationError)
//             req.flash('errorMsg', 'Softlocks konnten nicht storniert werden. Bitte wende dich an die Raidleitung.');
//         else
//             req.flash('errorMsg', 'Interner Serverfehler.');
//     } finally {
//         res.redirect('back');
//     }
// });

createHttpServer(app, 12345);