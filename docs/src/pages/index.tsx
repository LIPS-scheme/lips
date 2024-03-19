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
const DownArrow = require('@site/static/img/down-arrow.svg').default;

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title} v.&nbsp;<Version />
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <Interpreter/>
        <div className={styles.arrow}>
          <DownArrow />
        </div>
      </div>
    </header>
  );
}

export default function Home(): JSX.Element {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title={siteConfig.title}
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
