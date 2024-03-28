const DownArrow = require('@site/static/img/down-arrow.svg').default;
import styles from './index.module.css';

export default function MoreIcon(): JSX.Element {
  return (
    <div className={styles.arrow}>
      <DownArrow />
    </div>
  );
};
