import { Database } from 'sqlite3';


/*
Modes
0 - registration
1 - showing tables
*/

export interface UserData {
    userName: string;
    class: string;
    role: string;
    registeredOn: Date;
}

interface SoftlockItem {
    itemName?: string
}

interface SoftlockExtension {
    softlocks: SoftlockItem[]
}

export interface Raid {
    title: string;
    raidUserKey: string;
    dungeonKey: string|null;
    numPriorities: number;
    mode: number;
    createdOn: Date;
    comments?: string;
}

export class RegistrationError extends Error {

}

export class CancellationError extends Error {

}

export class InternalError extends Error {

}

export class NoSuchRaidError extends Error {

}

const db = new Database('database.sqlite');

async function get(query: TemplateStringsArray, ...args: any[]): Promise<any> {
    return new Promise((resolve, reject) => {
        db.get(query.join('?'), args, (err, row) => {
            if(err) return reject(err);
            resolve(row);
        });
    });
}

async function run(query: TemplateStringsArray, ...args: any[]): Promise<void> {
    return new Promise((resolve, reject) => {
        db.run(query.join('?'), args, err => {
            if(err) return reject(err);
            resolve();
        });
    });
}

async function all(query: TemplateStringsArray, ...args: any[]): Promise<any[]> {
    return new Promise((resolve, reject) => {
        db.all(query.join('?'), args, (err, rows) => {
            if(err) return reject(err);
            resolve(rows);
        });
    });
}

async function transaction<T>(fun: () => Promise<T>): Promise<T> {
    try {
        await run`BEGIN`;
        let result = await fun();
        await run`COMMIT`;
        return result;
    } catch(err) {
        await run`ROLLBACK`;
        throw err;
    }
}

export async function initialize() {
    await Promise.all([
        run`
            CREATE TABLE IF NOT EXISTS raids(
                raid_id INTEGER PRIMARY KEY AUTOINCREMENT,
                raid_user_key TEXT NOT NULL UNIQUE,
                raid_admin_key TEXT NOT NULL UNIQUE,
                title TEXT NOT NULL,
                dungeon_key TEXT,
                num_priorities INTEGER NOT NULL,
                mode INTEGER NOT NULL DEFAULT 0,
                created_on TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                comments TEXT
            )`,
        run`
            CREATE TABLE IF NOT EXISTS users(
                raid_id INTEGER NOT NULL,
                user_name TEXT NOT NULL,
                user_id TEXT NOT NULL,
                class TEXT NOT NULL,
                role TEXT NOT NULL,
                registered_on TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY(raid_id, user_id),
                UNIQUE(raid_id, user_name)
            )`,
        run`
            CREATE TABLE IF NOT EXISTS softlocks(
                raid_id INTEGER NOT NULL,
                user_id TEXT NOT NULL,
                priority INTEGER NOT NULL,
                item_name TEXT NOT NULL,
                PRIMARY KEY(raid_id, user_id, priority)
            )`
    ])
}

export async function registerUser(raidUserKey: string, userName: string, userID: string, userClass: string, role: string, softlocks: SoftlockItem[]) {
    
    let row = await get`
        SELECT
            raid_id, num_priorities
        FROM raids
        WHERE raids.raid_user_key = ${raidUserKey}
        `;
    if(!row || row.num_priorities != softlocks.length) throw new RegistrationError();
    let raidID = row.raid_id;

    row = await get`
        SELECT
            user_id
        FROM users
        WHERE raid_id = ${raidID} AND (
            (user_name = ${userName} AND user_id != ${userID}) OR
            (user_name != ${userName} AND user_id = ${userID}))
        `;
    if(row) throw new RegistrationError();
    
    await transaction(() => 
        Promise.all([
            run`
                INSERT INTO users(raid_id, user_name, user_id, class, role)
                VALUES(${raidID}, ${userName}, ${userID}, ${userClass}, ${role})
                `,
            Promise.all(softlocks.map((item, index) =>
                item.itemName == null ?
                    Promise.resolve()
                :
                    run`
                        INSERT INTO softlocks(raid_id, user_id, priority, item_name)
                        VALUES(${raidID}, ${userID}, ${index+1}, ${item.itemName})
                        `
            ))
        ])
    );
}

export async function getRaid(key: string): Promise<Raid|null> {
    let row = await get`
        SELECT raid_id, raid_user_key, title, dungeon_key, num_priorities, mode, created_on, comments
        FROM raids
        WHERE raid_admin_key = ${key} OR raid_user_key = ${key}
        `;
    if(!row) return null;
    return {
        title: row.title,
        raidUserKey: row.raid_user_key,
        dungeonKey: row.dungeon_key,
        numPriorities: row.num_priorities,
        mode: row.mode,
        createdOn: new Date(row.created_on + 'Z'),
        comments: row.comments
    };
}

export async function getRestrictedUserList(key: string): Promise<UserData[]> {
    let userList = await all`
        SELECT user_name as userName, class, role, registered_on as registeredOn
        FROM users
            INNER JOIN raids on users.raid_id = raids.raid_id
        WHERE
            raids.raid_user_key = ${key} OR
            raids.raid_admin_key = ${key}
        `;
    return userList.map(data => ({
        ...data,
        registeredOn: new Date(data.registeredOn + 'Z')
    }))
}

export async function getCompleteUserList(raidUserKey: string): Promise<(UserData&SoftlockExtension)[]> {
    let raid = await get`
        SELECT raid_id, num_priorities
        FROM raids
        WHERE raid_user_key = ${raidUserKey}
        `;
    if(!raid) throw new NoSuchRaidError();
    
    let userList = await all`
        SELECT user_name, user_id, class, role, registered_on
        FROM users
            INNER JOIN raids on users.raid_id = raids.raid_id
        WHERE
            raids.raid_user_key = ${raidUserKey}
        `;

    return await Promise.all(userList.map(async data => ({
        userName: data.user_name,
        class: data.class,
        role: data.role,
        registeredOn: new Date(data.registered_on + 'Z'),
        softlocks: await getSoftlocks(raid.raid_id, data.user_id, raid.num_priorities)
    })))
}

export async function getUserData(raidUserKey: string, userID: string): Promise<UserData&SoftlockExtension|null> {
    let row = await get`
            SELECT user_name, class, role, registered_on, users.raid_id, num_priorities
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.raid_user_key = ${raidUserKey} AND users.user_id = ${userID}
        `;
    if(!row) return null;
    return {
        userName: row.user_name,
        class: row.class,
        role: row.role,
        registeredOn: new Date(row.registered_on + 'Z'),
        softlocks: await getSoftlocks(row.raid_id, userID, row.num_priorities)
    }
}

async function getSoftlocks(raidID: number, userID: string, numPriorities: number): Promise<SoftlockItem[]> {
    let rows = await all`
        SELECT priority, item_name
        FROM softlocks
        WHERE raid_id = ${raidID} AND user_id = ${userID}
        `;
    let softlocks = Array(numPriorities).fill(null);
    for(let row of rows) {
        if(row.priority < 1 || row.priority > numPriorities)
            throw new InternalError();
        softlocks[row.priority - 1] = row.item_name;
    }
    return softlocks.map(name => ({ itemName: name }));
}
    
export async function removeUser(raidUserKey: string, userID: string) {
    try{
        let row = await get`
            SELECT raid_id
            FROM raids
            WHERE raid_user_key = ${raidUserKey}
            `;
        if(!row) throw new Error();
        await _removeUser(row.raid_id, userID);
    } catch {
        throw new CancellationError();
    }
}
    
export async function adminRemoveUser(raidAdminKey: string, userName: string) {
    try {
        let row = await get`
            SELECT raids.raid_id, user_id
            FROM raids
                INNER JOIN users ON raids.raid_id = users.raid_id
            WHERE raid_admin_key = ${raidAdminKey} AND user_name = ${userName}
            `;
        if(!row) return;
        await _removeUser(row.raid_id, row.user_id);
    } catch {
        throw new CancellationError();
    }
}

async function _removeUser(raidID: number, userID: string) {
    await transaction(async () => {
        await run`
            DELETE FROM users
            WHERE user_id = ${userID} AND raid_id = ${raidID}
            `;
        await run`
            DELETE FROM softlocks
            WHERE user_id = ${userID} AND raid_id = ${raidID}
            `;
        }
    )
}

export async function close() {
    return new Promise((resolve, reject) => {
        db.close(err => {
            if(err)
                return reject(err);
            resolve();
        });
    });
}

async function _createRaid(title: string, raidUserKey: string, raidAdminKey: string, numPriorities: number, dungeonKey?: string, comments?: string) {
    try{
        await run`
            INSERT INTO raids(title, raid_user_key, raid_admin_key, dungeon_key, comments, num_priorities)
            VALUES(${title}, ${raidUserKey}, ${raidAdminKey}, ${dungeonKey}, ${comments}, ${numPriorities})
            `;
    } catch {
        throw new InternalError();
    }
}

export async function createRaid(title: string, numPriorities: number, dungeonKey?: string, comments?: string) {
    while(true) {
        try {
            let raidAdminKey = randomString(20);
            let raidUserKey = randomString(6);
            await _createRaid(title, raidUserKey, raidAdminKey, numPriorities, dungeonKey, comments);
            return raidAdminKey;
        } catch(err) {
            if(!(err instanceof InternalError))
                throw err;
        }
    }
}

export async function setRaidMode(raidAdminKey: string, mode: number) {
    try {
        await run`UPDATE raids SET mode = ${mode} WHERE raid_admin_key = ${raidAdminKey}`
    } catch {
        throw new InternalError();
    }
}

function randomString(length: number) {
    let result = '';
    let characters = 'ABCDEFGHJKLMNOPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
    for ( var i = 0; i < length; i++ ) {
       result += characters.charAt(Math.floor(Math.random() * characters.length));
    }
    return result;
}