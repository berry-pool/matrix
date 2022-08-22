export const onCreateWebpackConfig = ({ actions }: any) => {
  actions.setWebpackConfig({
    experiments: {
      asyncWebAssembly: true,
      topLevelAwait: true,
    },
  });
};
