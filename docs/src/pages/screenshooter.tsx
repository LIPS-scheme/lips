import Layout from '@theme/Layout';
import Heading from '@theme/Heading';

import ScreenShotBox from '@site/src/components/ScreenShotBox';

export default function Screenshooter(): JSX.Element {
  return (
    <Layout
      title={'Screenshooter'}
      description={'Grab a screenshot of LIPS Scheme source code to share on social media'}>
      <main>
          <Heading as="h2" className="screenshot-header">Write LIPS Scheme code and grab a screenshot</Heading>
          <ScreenShotBox />
      </main>
    </Layout>
  );
}
