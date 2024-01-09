const { useRef, useState, useEffect, useMemo } = React;

const fuse_options = {
    includeScore: true,
    threshold: 0.1,
    keys: [
        'doc',
        {
            name: 'name',
            weight: 2
        }
    ]
};

const App = () => {
    const [term, setTerm] = useState('');
    const [docs, setDocs] = useState({data: [], index: null});

    const default_list = useMemo(() => {
        return docs.data.map(item => ({item}));
    }, [docs]);
    
    const length = useMemo(() => {
        const lengths = docs.data.map(({doc}) => {
            const lenghts = doc.split('\n').map(line => line.length);
            return Math.max(...lenghts);
        });
        return Math.max(...lengths);
        
    }, [docs]);
    
    useEffect(() => {
        const data = get_docs_strings();
        data.sort((a, b) => a.name.localeCompare(b.name));
        const index = Fuse.createIndex(fuse_options.keys, data)
        setDocs({
            data,
            index
        });
    }, []);
    
    function handleChange(event) {
        setTerm(event.target.value);
    }

    const fuse = new Fuse(docs.data, fuse_options, docs.index);

    const result = term.trim() ? fuse.search(term) : default_list;

    return (
        <div id="search" style={{'--length': length}}>
            <div className="input">
                <label htmlFor="term">Search</label>
                <input
                    id="term"
                    onChange={handleChange}
                    value={term}
                 />
            </div>
            <ul>
                {result.map(({item}) => {
                    
                    return (
                        <li key={item.name}>
                            <h2>{ item.name }</h2>
                            <pre>{ item.doc }</pre>
                        </li>
                    );
                })}
            </ul>
        </div>
    );
}


const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(<App/>);


function skip_internal([name]) {
    return name.match(/^%/) === null;
}

function map_docs(pairs) {
    return pairs.filter(skip_internal).map(([name, fn]) => {
        return {
            name,
            doc: fn?.__doc__
        };
    }).filter(object => object.doc);
}
function get_docs_strings() {
    const env = lips.env.__parent__.__env__;
    return map_docs(Object.entries(env));
}