import './nice-select';
import ElmRegister from './Register.elm';
import ElmTables from './Tables.elm';
import ElmAdmin from './Admin.elm';
import ElmIndex from './Index.elm';

window.Elm = {
    Register: ElmRegister.Elm.Register,
    Tables: ElmTables.Elm.Tables,
    Admin: ElmAdmin.Elm.Admin,
    Index: ElmIndex.Elm.Index
};

const userIDLength = 50;

window.getUserID = () => {
    let id = localStorage.getItem('user-id')
    if(!id || id.length != userIDLength) {
        id = randomString(userIDLength);
        localStorage.setItem('user-id', id);
    }
    return id;
};

function randomString(length) {
    let result = '';
    let characters = 'ABCDEFGHJKLMNOPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
    for ( var i = 0; i < length; i++ ) {
       result += characters.charAt(Math.floor(Math.random() * characters.length));
    }
    return result;
}