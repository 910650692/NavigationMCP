package com.fy.navi.scene.impl.search;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneMainSearchView;
import com.fy.navi.scene.ui.search.SceneMainSearchTopPartView;
import com.fy.navi.ui.base.StackManager;

public class SceneMainSearchViewImpl extends BaseSceneModel<SceneMainSearchTopPartView> implements ISceneMainSearchView {
    public SceneMainSearchViewImpl(SceneMainSearchTopPartView mScreenView) {
        super(mScreenView);
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    @Override
    public void onClickQuickSearch(int position) {
        mScreenView.onClickQuickSearch(position);
    }
}
