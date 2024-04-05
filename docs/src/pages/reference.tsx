import { CSSProperties, useMemo } from 'react';

import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import Head from '@docusaurus/Head';

import styles from './index.module.css';
import docs from '@site/reference.json';
import Reference from '@site/src/components/Reference';

interface CustomStyle extends CSSProperties {
  '--length': number;
}

export default function ReferencePage(): JSX.Element {
  const length = useMemo(() => {
    const lengths = docs.map(({doc}) => {
      const lengths = doc.split('\n').map((line: string) => line.length);
      return Math.max(...lengths);
    });
    return Math.max(...lengths);

  }, []);

  const style: CustomStyle = {'--length': length};

  return (
    <Layout
      title={'Function and Macro Reference'}
      description={'This is list of functions and macros that are part of LIPS Scheme implementation'}>
      <Head>
        <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"
                data-bootstrap="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/std.xcb"></script>
      </Head>
      <main className={styles.container} style={style}>
        <Heading as="h2" className={styles.header}>LIPS Scheme Function and Macro Reference</Heading>
        <Reference docs={docs} />
      </main>
    </Layout>
  );
}
