import express from 'express';
import session from 'express-session';
import { CapacityReachedError, RegistrationError, CancellationError, getUserData, getRaid, removeUser, registerUser, setRaidMode, getRestrictedUserList, createRaid } from './database';
import { Field, parse, MaxLength, MinLength, JsonParseError, Min, Max } from 'sparkson';

let app = express.Router();
app.use(express.json());
app.use(session({ secret: 'sleeplocker', resave: false, saveUninitialized: false, cookie: { secure: false, maxAge: 86400000, sameSite: true } }))

class UserKeyQueryData {
    constructor(
        @Field("userKey") @MinLength(6) @MaxLength(6) public userKey: string,
    ) { }
}

app.post('/myData', async (req, res) => {
    try {
        let userKey = parse(UserKeyQueryData, req.body).userKey;
        let session = req.session as Express.Session;
        let userName = session[userKey] as string | undefined;
        let raid = await getRaid(userKey);
        if (userName == undefined || raid == undefined)
            return res.json(fail('Nichts gefunden.'));
        if (raid.mode != 0)
            return res.json(fail('Anmeldung ist nicht mehr möglich.'))
        let userData = await getUserData(userKey, userName);
        if (userData == null)
            return res.json(fail('Nichts gefunden.'));
        return res.json(succeed(userData));
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'))
    }
});

app.post('/getRaid', async (req, res) => {
    try {
        let userKey = parse(UserKeyQueryData, req.body).userKey;
        let raid = await getRaid(userKey);
        if (raid == undefined)
            return res.json(fail('Raid nicht gefunden.'));
        return res.json(succeed(raid));
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'))
    }
});

app.post('/clearMyData', async (req, res) => {
    try {
        let userKey = parse(UserKeyQueryData, req.body).userKey;
        let session = req.session as Express.Session;
        let userName = session[userKey] as string | undefined;
        let raid = await getRaid(userKey);
        if (userName == undefined || raid == undefined)
            return res.json(fail('Nichts gefunden.'));
        if (raid.mode != 0)
            return res.json(fail('Stornieren ist nicht mehr möglich.'))
        await removeUser(userKey, userName);
        session[userKey] = undefined;
        return res.json(succeed());
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        if (err instanceof CancellationError)
            return res.json(fail('Stornieren fehlgeschlagen.'));
        return res.json(fail('Interner Serverfehler.'))
    }
});


class RegisterData {
    constructor(
        @Field("userName") @MinLength(1) @MaxLength(50) public userName: string,
        @Field("userKey") @MinLength(6) @MaxLength(6) public userKey: string,
        @Field("class") @MinLength(1) @MaxLength(50) public characterClass: string,
        @Field("role") @MinLength(1) @MaxLength(50) public role: string,
        @Field("prio1", true) @MinLength(1) @MaxLength(50) public prio1?: string,
        @Field("prio2", true) @MinLength(1) @MaxLength(50) public prio2?: string,
    ) { }
}

app.post('/register', async (req, res) => {
    try {
        let data = parse(RegisterData, req.body);
        let raid = await getRaid(data.userKey);
        if (raid == null)
            return res.json(fail('Raid nicht gedunden.'));
        if (raid.mode != 0)
            return res.json(fail('Anmeldung ist nicht mehr möglich.'))
        await registerUser(data.userKey, data.userName, data.characterClass, data.role, data.prio1, data.prio2);
        (req.session as Express.Session)[data.userKey] = data.userName;
        return res.json(succeed());
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        if (err instanceof CapacityReachedError)
            return res.json(fail('Die maximale Anzahl an Softlocks wurde erreicht.'));
        else if (err instanceof RegistrationError)
            return res.json(fail('Softlocks konnten nicht angenommen werden. Bitte wende dich an die Raidleitung.'));
        return res.json(fail('Interner Serverfehler.'));
    }
});


class RaidModeData {
    constructor(
        @Field("adminKey") @MinLength(20) @MaxLength(20) public adminKey: string,
        @Field("mode") @Min(0) @Max(1) public mode: number
    ) { }
}

app.post('/setMode', async (req, res) => {
    try {
        let data = parse(RaidModeData, req.body);
        await setRaidMode(data.adminKey, data.mode);
        return res.json(succeed());
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'));
    }
});


class RegistrationsQueryData {
    constructor(
        @Field("adminKey") @MinLength(20) @MaxLength(20) public adminKey: string
    ) { }
}

app.post('/getRegistrations', async (req, res) => {
    try {
        let adminKey = parse(RegistrationsQueryData, req.body).adminKey;
        let userList = await getRestrictedUserList(adminKey);
        let projectedList = userList.map(data => ({
            userName: data.userName,
            class: data.class,
            role: data.role
        }));
        return res.json(succeed(projectedList));
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'));
    }
});


class NewRaidData {
    constructor(
        @Field("title") @MinLength(1) @MaxLength(50) public title: string,
        @Field("dungeonKey", true) @MinLength(1) @MaxLength(50) public dungeonKey?: string,
        @Field("comments", true) @MinLength(1) @MaxLength(1000) public comments?: string
    ) { }
}

app.post('/createRaid', async (req, res) => {
    try {
        let data = parse(NewRaidData, req.body);
        let adminKey = await createRaid(data.title, data.dungeonKey, data.comments);
        return res.json(succeed(adminKey));
    } catch(err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'));
    }
});

interface ApiResponse<T> {
    success: Boolean;
    errorMsg: String | null;
    result: T | null;
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