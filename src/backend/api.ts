import express from 'express';
import { RegistrationError, CancellationError, getUserData, getRaid, removeUser, registerUser, setRaidMode, getRestrictedUserList, createRaid } from './database';
import { Field, parse, MaxLength, MinLength, JsonParseError, Min, Max } from 'sparkson';
import _ from 'lodash';

let app = express.Router();
app.use(express.json());

class UserQueryData {
    constructor(
        @Field("raidUserKey") @MinLength(6) @MaxLength(6) public raidUserKey: string,
        @Field("userID") @MinLength(50) @MaxLength(50) public userID: string
    ) { }
}

app.post('/myData', async (req, res) => {
    try {
        let data = parse(UserQueryData, req.body);
        let raid = await getRaid(data.raidUserKey);
        if (raid == undefined)
            return res.json(fail('Raid nicht gefunden.'));
        if (raid.mode != 0)
            return res.json(fail('Anmeldung ist nicht mehr möglich.'))
        let userData = await getUserData(data.raidUserKey, data.userID);
        if (userData == null)
            return res.json(fail('Keine Anmeldung gefunden.'));
        return res.json(succeed(userData));
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'))
    }
});

class RaidQueryData {
    constructor(
        @Field("raidUserKey") @MinLength(6) @MaxLength(6) public raidUserKey: string,
    ) { }
}

app.post('/getRaid', async (req, res) => {
    try {
        let raidUserKey = parse(RaidQueryData, req.body).raidUserKey;
        let raid = await getRaid(raidUserKey);
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
        let data = parse(UserQueryData, req.body);
        let raid = await getRaid(data.raidUserKey);
        if (raid == undefined)
            return res.json(fail('Raid nicht gefunden.'));
        if (raid.mode != 0)
            return res.json(fail('Stornieren ist nicht mehr möglich.'))
        await removeUser(data.raidUserKey, data.userID);
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
        @Field("userID") @MinLength(50) @MaxLength(50) public userID: string,
        @Field("raidUserKey") @MinLength(6) @MaxLength(6) public raidUserKey: string,
        @Field("class") @MinLength(1) @MaxLength(50) public characterClass: string,
        @Field("role") @MinLength(1) @MaxLength(50) public role: string,
        @Field("prio1", true) @MinLength(1) @MaxLength(50) public prio1?: string,
        @Field("prio2", true) @MinLength(1) @MaxLength(50) public prio2?: string,
    ) { }
}

app.post('/register', async (req, res) => {
    try {
        let data = parse(RegisterData, req.body);
        let raid = await getRaid(data.raidUserKey);
        if (raid == null)
            return res.json(fail('Raid nicht gedunden.'));
        if (raid.mode != 0)
            return res.json(fail('Anmeldung ist nicht mehr möglich.'))
        await registerUser(data.raidUserKey, data.userName, data.userID, data.characterClass, data.role, data.prio1, data.prio2);
        return res.json(succeed());
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        else if (err instanceof RegistrationError)
            return res.json(fail('Anmeldung fehlgeschlagen. Bitte wende dich an die Raidleitung.'));
        return res.json(fail('Interner Serverfehler.'));
    }
});


class RaidModeData {
    constructor(
        @Field("raidAdminKey") @MinLength(20) @MaxLength(20) public raidAdminKey: string,
        @Field("mode") @Min(0) @Max(1) public mode: number
    ) { }
}

app.post('/setMode', async (req, res) => {
    try {
        let data = parse(RaidModeData, req.body);
        await setRaidMode(data.raidAdminKey, data.mode);
        return res.json(succeed());
    } catch (err) {
        if (err instanceof JsonParseError)
            return res.json(fail('Ungültige Eingabe.'));
        return res.json(fail('Interner Serverfehler.'));
    }
});


class RegistrationsQueryData {
    constructor(
        @Field("raidAdminKey") @MinLength(20) @MaxLength(20) public raidAdminKey: string
    ) { }
}

app.post('/getRegistrations', async (req, res) => {
    try {
        let raidAdminKey = parse(RegistrationsQueryData, req.body).raidAdminKey;
        let userList = await getRestrictedUserList(raidAdminKey);
        userList = _.sortBy(userList, user => user.registeredOn);
        let projectedList = userList.map(data => ({
            userName: data.userName,
            class: data.class,
            role: data.role,
            registeredOn: data.registeredOn.toLocaleString()
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
        let raidAdminKey = await createRaid(data.title, data.dungeonKey, data.comments);
        return res.json(succeed(raidAdminKey));
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