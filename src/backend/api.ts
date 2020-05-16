import express, { RequestHandler } from 'express';
import session from 'express-session';
import { RaidDatabase, SoftlockCapacityReachedError, SoftlockRegistrationError } from './database';
import { Field, parse, MaxLength, MinLength, JsonParseError } from 'sparkson';

let db = new RaidDatabase();
let app = express.Router();
app.use(express.json());
app.use(session({secret: 'sleeplocker', resave: false, saveUninitialized: false, cookie: { secure: false, maxAge: 86400000 }}))
db.initialize().then(() => {
    app.get('/myData', getMyData);
    app.post('/register', registerData);
});

let getMyData: RequestHandler = async (req, res) => {
    try {
        let raidID = await db.currentRaidId();
        let name: string|undefined = (req.session as Express.Session).name;
        if(name == undefined || raidID == null)
            return res.json(fail('Nothing found.'));
        let raid = await db.getRaid(raidID);
        if(raid.mode != 1)
            return res.json(fail('Not allowed in this state.'))
        let data = await db.getUserData(raidID, name);
        if(data == null)
            return res.json(fail('Nothing found.'));
        return res.json(succeed(data));
    } catch {
        return res.json(fail('Internal server error.'))
    }
}

let registerData: RequestHandler = async (req, res) => {
    try {
        let data = parse(RegisterData, req.body);
        let raidID = await db.currentRaidId();
        if(raidID == null)
            return res.json(fail('No active raid.'));
        let raid = await db.getRaid(raidID);
        if(raid.mode != 1)
            return res.json(fail('Not allowed in this state.'))
        await db.setUserLocks(raidID, data.userName, data.characterClass, data.specialization, data.prio1, data.prio2);
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
        @Field("specialization") @MinLength(1) @MaxLength(50) public specialization: string,
        @Field("prio1") @MinLength(1) @MaxLength(50) public prio1?: string,
        @Field("prio2") @MinLength(1) @MaxLength(50) public prio2?: string,
    ) {}
}

interface ApiResponse<T> {
    success: Boolean;
    errorMsg?: String;
    result?: T;
}

function succeed<T>(result?: T): ApiResponse<T> {
    return {
        success: true,
        result: result
    }
}

function fail<T>(errorMsg: String): ApiResponse<T> {
    return {
        success: false,
        errorMsg: errorMsg
    }
}

export default app;