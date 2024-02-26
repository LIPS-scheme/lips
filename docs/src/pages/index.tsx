import clsx from 'clsx';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Interpreter from '@site/src/components/Interpreter';
import Tools from '@site/src/components/Tools';
import Heading from '@theme/Heading';
import Version from '@site/src/components/Version';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title} v. <Version />
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <Interpreter/>
      </div>
    </header>
  );
}

import Video from '@site/src/components/Video';

export default function Home(): JSX.Element {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title={siteConfig.title}
      description={siteConfig.tagline}>
      <HomepageHeader />
      <main>
        <Tools />
        <Video />
      </main>
    </Layout>
  );
}
