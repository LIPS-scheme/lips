import Heading from '@theme/Heading';

import styles from '../styles.module.css';

export default function Video(): JSX.Element {
  return (
    <div className={styles.section} id="video">
      <Heading as="h2" id="video" className={styles.header}>Video Presentation about LIPS Scheme</Heading>
      <p>
        Video presentation given during <a href="https://archive.fosdem.org/2023/">FOSDEM 2023</a> in <a href="https://archive.fosdem.org/2023/schedule/track/declarative_and_minimalistic_computing/">Declarative and Minimalistic Computing devroom</a>. It discuss different aspect of LIPS Scheme. It first gives quick intro to Lisp and Scheme and later show different features of LIPS Scheme.
      </p>
      <video controls preload="none" poster="./img/fosdem-intro.png">
        <source src="https://video.fosdem.org/2023/D.minimalistic/lipsscheme.webm" type="video/webm; codecs=&quot;vp9, opus&quot;"/>
        <source src="https://video.fosdem.org/2023/D.minimalistic/lipsscheme.mp4" type="video/mp4"/>
      </video>
    </div>
  );
}
