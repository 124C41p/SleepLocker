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
    prio1: string|null;
    prio2: string|null;
    registeredOn: Date;
}

export interface Raid {
    title: string;
    raidUserKey: string;
    dungeonKey: string|null;
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

const db = new Database('database.sqlite');

export async function initialize() {
    return new Promise((resolve, reject) => {
        db.serialize(() => {
            db.exec(`
                CREATE TABLE IF NOT EXISTS raids(
                    raid_id INTEGER PRIMARY KEY AUTOINCREMENT,
                    raid_user_key TEXT NOT NULL UNIQUE,
                    raid_admin_key TEXT NOT NULL UNIQUE,
                    title TEXT NOT NULL,
                    dungeon_key TEXT,
                    mode INTEGER NOT NULL DEFAULT 0,
                    created_on TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    comments TEXT
                )
            `, err => {
                if(err) return reject(err);
            });
            db.exec(`
                CREATE TABLE IF NOT EXISTS users(
                    raid_id INTEGER NOT NULL,
                    user_name TEXT NOT NULL,
                    user_id TEXT NOT NULL,
                    class TEXT NOT NULL,
                    role TEXT NOT NULL,
                    prio1 TEXT,
                    prio2 TEXT,
                    registered_on TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    PRIMARY KEY(raid_id, user_id),
                    UNIQUE(raid_id, user_name)
                )
            `, err => {
                if(err) return reject(err);
                resolve();
            });
        });
    });
}

export async function getUserData(raidUserKey: string, userID: string): Promise<UserData|null> {
    return new Promise((resolve, reject) => {
        db.get(`
            SELECT user_name, class, role, prio1, prio2, registered_on
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.raid_user_key = ? AND users.user_id = ?
        `, [raidUserKey, userID], (err, row) => {
            if(err) return reject(err);
            if(!row) return resolve(null);
            resolve({
                userName: row.user_name,
                class: row.class,
                role: row.role,
                prio1: row.prio1,
                prio2: row.prio2,
                registeredOn: new Date(row.registered_on + 'Z')
            });
        });
    });
}

export function registerUser(raidUserKey: string, userName: string, userID: string, userClass: string, role: string, prio1?: string, prio2?: string) {
    return new Promise((resolve, reject) => {
        db.get(`
            SELECT
                raid_id
            FROM raids
            WHERE raids.raid_user_key = ?
        `, [raidUserKey], (err, row) => {
            if(err) return reject(err);
            if(!row) return reject(new RegistrationError());
            let raidID = row.raid_id;

            db.serialize(() => {
                db.get(`
                    SELECT
                        user_id
                    FROM users
                    WHERE raid_id = ? AND (
                        (user_name = ? AND user_id != ?) OR
                        (user_name != ? AND user_id = ?))
                `, [raidID, userName, userID, userName, userID], (err, row) => {
                    if(err) return reject(err);
                    if(row)
                        return reject(new RegistrationError());
                })

                db.run(`
                    INSERT INTO users(raid_id, user_name, user_id, class, role, prio1, prio2)
                    VALUES(?,?,?,?,?,?,?)`, [raidID, userName, userID, userClass, role, prio1, prio2], err => {
                    if(err) return reject(err);
                    resolve();
                });
            });
        });
    });
}

export async function getRaid(key: string): Promise<Raid|null> {
    return new Promise((resolve, reject) => {
        db.get(`
            SELECT raid_id, raid_user_key, title, dungeon_key, mode, created_on, comments
            FROM raids WHERE raid_admin_key = ? OR raid_user_key = ?
        `, [key, key], (err, row) => {
            if(err) return reject(err);
            if(!row) return resolve(null);
            resolve({
                title: row.title,
                raidUserKey: row.raid_user_key,
                dungeonKey: row.dungeon_key,
                mode: row.mode,
                createdOn: new Date(row.created_on + 'Z'),
                comments: row.comments
            });
        });
    });
}

export async function getUserList(raidUserKey: string): Promise<UserData[]> {
    return new Promise((resolve, reject) => {
        db.all(`
            SELECT user_name as userName, class, role, prio1, prio2
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.raid_user_key = ?
        `, [raidUserKey], (err, rows) => {
            if(err) return reject(err);
            resolve(rows);
        });
    });
}

export async function getRestrictedUserList(raidAdminKey: string): Promise<UserData[]> {
    return new Promise((resolve, reject) => {
        db.all(`
            SELECT user_name as userName, class, role, registered_on as registeredOn
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.raid_admin_key = ?
        `, [raidAdminKey], (err, rows) => {
            if(err) return reject(err);
            resolve(rows.map(data => ({ ...data, registeredOn: new Date(data.registeredOn + 'Z') })));
        });
    });
}
    
export async function removeUser(raidUserKey: string, userID: string) {
    return new Promise((resolve, reject) => {
        db.run(`
            DELETE FROM users
            WHERE user_id = ? AND raid_id IN (
                SELECT raid_id
                FROM raids
                WHERE raid_user_key = ?
            )
        `, [userID, raidUserKey], err => {
            if(err) return reject(new CancellationError());
            resolve();
        });
    });
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

async function _createRaid(title: string, raidUserKey: string, raidAdminKey: string, dungeonKey?: string, comments?: string) {
    return new Promise((resolve, reject) => {
        db.run(`
            INSERT INTO raids(title, raid_user_key, raid_admin_key, dungeon_key, comments)
            VALUES(?,?,?,?,?)
        `, [title, raidUserKey, raidAdminKey, dungeonKey, comments], err => {
            if(err) return reject(new InternalError());
            resolve();
        });
    });        
}

export async function createRaid(title: string, dungeonKey?: string, comments?: string) {
    while(true) {
        try {
            let raidAdminKey = randomString(20);
            let raidUserKey = randomString(6);
            await _createRaid(title, raidUserKey, raidAdminKey, dungeonKey, comments);
            return raidAdminKey;
        } catch(err) {
            if(!(err instanceof InternalError))
                throw err;
        }
    }
}

export async function setRaidMode(raidAdminKey: string, mode: number) {
    return new Promise((resolve, reject) => {
        db.run('UPDATE raids SET mode=? WHERE raid_admin_key = ?', [mode, raidAdminKey], err => {
            if(err) return reject(new InternalError());
            resolve();
        });
    });
}

function randomString(length: number) {
    let result = '';
    let characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
    for ( var i = 0; i < length; i++ ) {
       result += characters.charAt(Math.floor(Math.random() * characters.length));
    }
    return result;
}