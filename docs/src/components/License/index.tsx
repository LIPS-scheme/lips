import Heading from '@theme/Heading';
import styles from '../styles.module.css';

export default function License(): JSX.Element {
    return (
      <div className={styles.section}>
        <Heading as="h2" className={styles.header}>License</Heading>
        <p>
          LIPS Scheme is <a href="https://opensource.com/resources/what-open-source">Open Source</a>
          {' '}is released on <a href="https://github.com/jcubic/lips/blob/master/LICENSE">MIT license</a></p>
      </div>
    );
}
