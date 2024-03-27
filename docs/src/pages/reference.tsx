import { useState, useEffect, useMemo, ChangeEvent, CSSProperties } from 'react';
import Fuse, { FuseIndex } from 'fuse.js'

import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import Head from '@docusaurus/Head';

import styles from './index.module.css';
import docs from '@site/reference.json';

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

export interface CustomStyle extends CSSProperties {
  '--length': number;
}

const Reference = () => {
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
              <h2>{ item.name }</h2>
              <pre>{ item.doc }</pre>
            </li>
          );
        })}
      </ul>
    </div>
  );
}

export default function ReferencePage(): JSX.Element {
  const { siteConfig } = useDocusaurusContext();

  const length = useMemo(() => {
    const lengths = docs.map(({doc}) => {
      const lenghts = doc.split('\n').map((line: string) => line.length);
      return Math.max(...lenghts);
    });
    return Math.max(...lengths);

  }, []);

  const style: CustomStyle = {'--length': length};

  return (
    <Layout
      title={siteConfig.title}
      description={siteConfig.tagline}>
      <Head>
        <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"
                data-bootstrap="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/std.xcb"></script>
      </Head>
      <main className={styles.container} style={style}>
        <Heading as="h2" className={styles.header}>LIPS Scheme Function and Macro Reference</Heading>
        <Reference />
      </main>
    </Layout>
  );
}
