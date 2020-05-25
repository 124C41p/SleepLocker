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