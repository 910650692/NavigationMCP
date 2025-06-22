package com.sgm.navi.scene.impl.search;

import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneMainSearchView;
import com.sgm.navi.scene.ui.search.SceneMainSearchTopPartView;
import com.sgm.navi.ui.base.StackManager;

public class SceneMainSearchViewImpl extends BaseSceneModel<SceneMainSearchTopPartView> implements ISceneMainSearchView {
    public SceneMainSearchViewImpl(final SceneMainSearchTopPartView screenView) {
        super(screenView);
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    @Override
    public void onClickQuickSearch(final int position) {
        mScreenView.onClickQuickSearch(position);
    }

    @Override
    public void onClickCollectSearch() {
        mScreenView.onClickCollectSearch();
    }
}
