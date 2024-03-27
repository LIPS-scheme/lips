import fs from 'fs/promises';

import { env, exec } from '../dist/lips.esm.js';

function skip_internal([name]) {
    return name.match(/^%/) === null;
}

function map_docs(pairs) {
    return pairs.filter(skip_internal).map(([name, obj]) => {
        return {
            name: name,
            doc: obj?.__doc__ && obj.__doc__.valueOf()
        };
    }).filter(object => object.doc);
}
function get_docs_strings() {
    const global_env = env.__parent__.__env__;
    const docs = map_docs(Object.entries(global_env));
    docs.sort((a, b) => a.name.localeCompare(b.name));
    return docs;
}

exec('(let-env lips.env.__parent__ (load "../dist/std.xcb"))').then(() => {
    const data = JSON.stringify(get_docs_strings());
    return fs.writeFile('reference.json', data);
}).catch(e => {
    console.log(e);
});
