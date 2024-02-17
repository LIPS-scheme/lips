import clsx from 'clsx';
import { useRef, useEffect } from 'react';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Interpreter from '@site/src/components/Interpreter';
import Bookmark from '@site/src/components/Bookmark';
import Heading from '@theme/Heading';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  const ref = useRef<HTMLSpanElement>();
  useEffect(() => {
    (function loop() {
      if (!(window as any).lips) {
        setTimeout(loop, 100);
      } else {
        ref.current.innerText = (window as any).lips.version;
      }
    })();
  }, []);
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title} v.<span ref={ref}></span>
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <Interpreter/>
      </div>
    </header>
  );
}

export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={siteConfig.title}
      description={siteConfig.tagline}>
      <HomepageHeader />
      <main>
        <Bookmark />
      </main>
    </Layout>
  );
}
