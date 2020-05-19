import fs from 'fs';
import _ from 'lodash';

export const characterClasses: CharacterClass[] = JSON.parse(fs.readFileSync('config/classes.json').toString());
const allLoot: DungeonLoot[] = JSON.parse(fs.readFileSync('config/loot.json').toString());
for(let dungeon of allLoot) {
    dungeon.loot = _.sortBy(dungeon.loot, item => item.itemName);
}
const uniqueLoot: DungeonUniqueLoot[] = allLoot.map(getUniqueLoot);

export interface CharacterClass {
    className: string;
    roles: string[];
}

interface DungeonLoot {
    dungeonKey: string;
    locations: string[];
    loot: Item[];
}

interface DungeonUniqueLoot {
    dungeonKey: string;
    loot: LootLocation[];
}

export interface LootLocation {
    locationName: string;
    loot: string[];
}

export interface Item {
    itemName: string;
    locations: string[];
}

function getUniqueLoot(dungeon: DungeonLoot): DungeonUniqueLoot {
    let items = dungeon.loot.map(item => ({ name: item.itemName, location: item.locations.length != 1 ? "Geteilter Loot" : item.locations[0] }));
    let groups = _.mapValues(_.groupBy(items, 'location'), itemList => _.sortBy(itemList.map(item => item.name)))
    let locations: LootLocation[] = _.map(groups, (items, location) => ({ locationName: location, loot: items }))
    return {
        dungeonKey: dungeon.dungeonKey,
        loot: _.sortBy(locations, location => dungeon.locations.indexOf(location.locationName)) };
}

export function getLootTable(dungeonKey: string): DungeonLoot|null {
    let dungeon = allLoot.filter(d => d.dungeonKey == dungeonKey)[0];
    if(dungeon == undefined)
        return null;
    return dungeon;
}

export function getLootLocations(dungeonKey: string): LootLocation[]|null {
    let dungeon = uniqueLoot.filter(d => d.dungeonKey == dungeonKey)[0];
    if(dungeon == undefined)
        return null;
    return dungeon.loot;
}