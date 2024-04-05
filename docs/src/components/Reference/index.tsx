import { useState, useEffect, useMemo, ChangeEvent } from 'react';
import Fuse, { FuseIndex } from 'fuse.js'
import Heading from '@theme/Heading';


import styles from './index.module.css';

const fuse_options = {
  includeScore: true,
  threshold: 0.1,
  ignoreLocation: true,
  keys: [
    'doc',
    {
      name: 'name',
      weight: 2
    }
  ]
};

type ReferenceProps = {
    docs: Array<{name: string, doc: string}>;
};

export default function Reference({ docs }: ReferenceProps) {
  const [term, setTerm] = useState('');
  const [index, setIndex] = useState<FuseIndex<{ name: string; doc: string; }>>();

  const default_list = useMemo(() => {
    return docs.map(item => ({item}));
  }, []);

  useEffect(() => {
    const index = Fuse.createIndex(fuse_options.keys, docs)
    setIndex(index);
  }, []);

  function handleChange(event: ChangeEvent<HTMLInputElement>) {
    setTerm(event.target.value);
  }

  const fuse = new Fuse(docs, fuse_options, index);

  const result = term.trim() ? fuse.search(term) : default_list;

  return (
    <div className={styles.search}>
      <div className={styles.input}>
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
              <Heading as="h2" id={item.name}>{ item.name }</Heading>
              <pre>{ item.doc }</pre>
            </li>
          );
        })}
      </ul>
    </div>
  );
}
