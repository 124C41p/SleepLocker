import { Database } from 'sqlite3';


/*
Modes
0 - receiving softlocks
1 - listing softlocks
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
    userKey: string;
    dungeonKey: string|null;
    mode: number;
    createdOn: Date;
    comments?: string;
}

export class RegistrationError extends Error {

}

export class CapacityReachedError extends Error {

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
                    user_key STRING NOT NULL UNIQUE,
                    admin_key STRING NOT NULL UNIQUE,
                    title STRING NOT NULL,
                    dungeon_key STRING,
                    mode INTEGER NOT NULL DEFAULT 0,
                    created_on TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    comments STRING
                )
            `, err => {
                if(err) return reject(err);
            });
            db.exec(`
                CREATE TABLE IF NOT EXISTS users(
                    raid_id INTEGER NOT NULL,
                    user_name STRING NOT NULL,
                    class STRING NOT NULL,
                    role STRING NOT NULL,
                    prio1 STRING,
                    prio2 STRING,
                    registered_on TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    PRIMARY KEY(raid_id, user_name)
                )
            `, err => {
                if(err) return reject(err);
                resolve();
            });
        });
    });
}

export async function getUserData(userKey: string, userName: string): Promise<UserData|null> {
    return new Promise((resolve, reject) => {
        db.get(`
            SELECT class, role, prio1, prio2, registered_on
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.user_key = ? AND users.user_name = ?
        `, [userKey, userName], (err, row) => {
            if(err) return reject(err);
            if(!row) return resolve(null);
            resolve({
                userName: userName,
                class: row.class,
                role: row.role,
                prio1: row.prio1,
                prio2: row.prio2,
                registeredOn: new Date(row.registered_on)
            });
        });
    });
}

export function registerUser(userKey: string, userName: string, userClass: string, role: string, prio1?: string, prio2?: string) {
    return new Promise((resolve, reject) => {
        db.get(`
            SELECT
                raid_id
            FROM raids
            WHERE raids.user_key = ?
        `, [userKey], (err, row) => {
            if(err || !row) return reject(new RegistrationError());
            let raidId = row.raid_id;

            db.serialize(() => {
                db.get(`
                    SELECT
                        COUNT(*) as no
                    FROM users
                    WHERE raid_id = ?
                `, [raidId], (err, row) => {
                    if(err) return reject(err);
                    if(row.no > 80) return reject(new CapacityReachedError());
                })

                db.run(`
                    INSERT INTO users(raid_id, user_name, class, role, prio1, prio2)
                    VALUES(?,?,?,?,?,?)`, [raidId, userName, userClass, role, prio1, prio2], err => {
                    if(err) return reject(new RegistrationError());
                    resolve();
                });
            });
        });
    });
}

export async function getRaid(key: string): Promise<Raid|null> {
    return new Promise((resolve, reject) => {
        db.get(`
            SELECT raid_id, user_key, title, dungeon_key, mode, created_on, comments
            FROM raids WHERE admin_key = ? OR user_key = ?
        `, [key, key], (err, row) => {
            if(err) return reject(err);
            if(!row) return resolve(null);
            resolve({
                title: row.title,
                userKey: row.user_key,
                dungeonKey: row.dungeon_key,
                mode: row.mode,
                createdOn: new Date(row.created_on),
                comments: row.comments
            });
        });
    });
}

export async function getUserList(userKey: string): Promise<UserData[]> {
    return new Promise((resolve, reject) => {
        db.all(`
            SELECT user_name as userName, class, role, prio1, prio2
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.user_key = ?
        `, [userKey], (err, rows) => {
            if(err) return reject(err);
            resolve(rows);
        });
    });
}

export async function getRestrictedUserList(adminKey: string): Promise<UserData[]> {
    return new Promise((resolve, reject) => {
        db.all(`
            SELECT user_name as userName, class, role
            FROM users
                INNER JOIN raids on users.raid_id = raids.raid_id
            WHERE raids.admin_key = ?
        `, [adminKey], (err, rows) => {
            if(err) return reject(err);
            resolve(rows);
        });
    });
}
    
export async function removeUser(userKey: string, userName: string) {
    return new Promise((resolve, reject) => {
        db.run(`
            DELETE FROM users
            WHERE user_name = ? AND raid_id IN (
                SELECT raid_id
                FROM raids
                WHERE user_key = ?
            )
        `, [userName, userKey], err => {
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

async function _createRaid(title: string, userKey: string, adminKey: string, dungeonKey?: string, comments?: string) {
    return new Promise((resolve, reject) => {
        db.run(`
            INSERT INTO raids(title, user_key, admin_key, dungeon_key, comments)
            VALUES(?,?,?,?,?)
        `, [title, userKey, adminKey, dungeonKey, comments], err => {
            if(err) return reject(new InternalError());
            resolve();
        });
    });        
}

export async function createRaid(title: string, dungeonKey?: string, comments?: string) {
    while(true) {
        try {
            let adminKey = randomString(20);
            let userKey = randomString(6);
            await _createRaid(title, userKey, adminKey, dungeonKey, comments);
            return adminKey;
        } catch(err) {
            if(!(err instanceof InternalError))
                throw err;
        }
    }
}

export async function setRaidMode(adminKey: string, mode: number) {
    return new Promise((resolve, reject) => {
        db.run('UPDATE raids SET mode=? WHERE admin_key = ?', [mode, adminKey], err => {
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