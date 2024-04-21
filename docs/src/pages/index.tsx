import clsx from 'clsx';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Heading from '@theme/Heading';
import Layout from '@theme/Layout';

import Interpreter from '@site/src/components/Interpreter';
import Tools from '@site/src/components/Tools';
import Version from '@site/src/components/Version';
import Acknowledgment from '@site/src/components/Acknowledgment';
import License from '@site/src/components/License';
import Video from '@site/src/components/Video';
import Intro from '@site/src/components/Intro';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title} v.&nbsp;<Version />
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <p className={styles.hidden}>Scheme Language Online REPL</p>
        <Interpreter/>
      </div>
    </header>
  );
}

export default function Home(): JSX.Element {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title={'Powerful Scheme interpreter in JavaScript'}
      description={siteConfig.tagline}>
      <HomepageHeader />
      <main>
        <Intro />
        <Tools />
        <Video />
        <Acknowledgment />
        <License />
      </main>
    </Layout>
  );
}
