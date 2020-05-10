import fs from 'fs';
import _ from 'lodash';

export const characterClasses: CharacterClass[] = JSON.parse(fs.readFileSync('config/classes.json').toString());
export const allLoot: Instance[] = JSON.parse(fs.readFileSync('config/loot.json').toString());
export const uniqueLoot: Instance[] = allLoot.map(ins => getUniqueLoot(ins));

export interface CharacterClass {
    name: string;
    specializations: string[];
}

export interface Instance {
    name: string;
    locations: LootLocation[];
}

export interface LootLocation {
    location: string;
    loot: string[];
}

interface Item {
    name: string;
    location: string;
}

function getUniqueLoot(instance: Instance): Instance {
    let allItems: Item[] = _.flatten(instance.locations.map(loc => loc.loot.map(itemName => ({ location: loc.location, name: itemName }))));
    let locationGroups = _.mapValues(_.groupBy(allItems, 'name'), itemList => itemList.map(item => item.location));
    let uniqueItems: Item[] = _.map(locationGroups, (group, itemName) => ({ name: itemName, location: group.length == 1 ? group[0] : 'Geteilter Loot' }))
    let itemGroups = _.groupBy(uniqueItems, 'location');
    let lootLocations = _.map(itemGroups, (items, location) => ({ location: location, loot: items.map(item => item.name) }))
    return { name: instance.name, locations: lootLocations };
}