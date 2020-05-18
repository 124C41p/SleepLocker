import express, { RequestHandler } from 'express';
import session from 'express-session';
import { RaidDatabase, SoftlockCapacityReachedError, SoftlockRegistrationError, SoftlockCancellationError } from './database';
import { Field, parse, MaxLength, MinLength, JsonParseError } from 'sparkson';

let db = new RaidDatabase();
let app = express.Router();
app.use(express.json());
app.use(session({secret: 'sleeplocker', resave: false, saveUninitialized: false, cookie: { secure: false, maxAge: 86400000, sameSite: true }}))
db.initialize().then(() => {
    app.get('/myData', getMyData);
    app.get('/clearMyData', clearMyData);
    app.post('/register', registerData);
});

let getMyData: RequestHandler = async (req, res) => {
    try {
        let raidID = await db.currentRaidId();
        let name: string|undefined = (req.session as Express.Session).name;
        if(name == undefined || raidID == null)
            return res.json(fail('Nichts gefunden.'));
        let raid = await db.getRaid(raidID);
        if(raid.mode != 1)
            return res.json(fail('Operation ist in diesem Zustand nicht erlaubt.'))
        let data = await db.getUserData(raidID, name);
        if(data == null)
            return res.json(fail('Nichts gefunden.'));
        return res.json(succeed(data));
    } catch {
        return res.json(fail('Interner Serverfehler.'))
    }
}

let clearMyData: RequestHandler = async (req, res) => {
    try {
        let raidID = await db.currentRaidId();
        let name: string|undefined = (req.session as Express.Session).name;
        if(name == undefined || raidID == null)
            return res.json(fail('Nichts gefunden.'));
        let raid = await db.getRaid(raidID);
        if(raid.mode != 1)
            return res.json(fail('Operation ist in diesem Zustand nicht erlaubt..'))
        await db.removeLock(raidID, name);
        return res.json(succeed());
    } catch(err) {
        if(err instanceof SoftlockCancellationError)
            return res.json(fail('Stornieren fehlgeschlagen.'));
        return res.json(fail('Interner Serverfehler.'))
    }
};

let registerData: RequestHandler = async (req, res) => {
    try {
        let data = parse(RegisterData, req.body);
        let raidID = await db.currentRaidId();
        if(raidID == null)
            return res.json(fail('Kein aktiver Raid.'));
        let raid = await db.getRaid(raidID);
        if(raid.mode != 1)
            return res.json(fail('Operation ist in diesem Zustand nicht erlaubt.'))
        await db.setUserLocks(raidID, data.userName, data.characterClass, data.role, data.prio1, data.prio2);
        (req.session as Express.Session).name = data.userName;
        return res.json(succeed());        
    } catch(err) {
        if(err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        if(err instanceof SoftlockCapacityReachedError)
            return res.json(fail('Die maximale Anzahl an Softlocks wurde erreicht.'));
        else if(err instanceof SoftlockRegistrationError)
            return res.json(fail('Softlocks konnten nicht angenommen werden. Bitte wende dich an die Raidleitung.'));
        return res.json(fail('Interner Serverfehler.'));
    }
}

class RegisterData {
    constructor(
        @Field("userName") @MinLength(1) @MaxLength(50) public userName: string,
        @Field("class") @MinLength(1) @MaxLength(50) public characterClass: string,
        @Field("role") @MinLength(1) @MaxLength(50) public role: string,
        @Field("prio1", true) @MinLength(1) @MaxLength(50) public prio1?: string,
        @Field("prio2", true) @MinLength(1) @MaxLength(50) public prio2?: string,
    ) {}
}

interface ApiResponse<T> {
    success: Boolean;
    errorMsg: String|null;
    result: T|null;
}

function succeed<T>(result?: T): ApiResponse<T> {
    return {
        success: true,
        errorMsg: null,
        result: result ?? null
    }
}

function fail<T>(errorMsg: String): ApiResponse<T> {
    return {
        success: false,
        errorMsg: errorMsg,
        result: null
    }
}

export default app;