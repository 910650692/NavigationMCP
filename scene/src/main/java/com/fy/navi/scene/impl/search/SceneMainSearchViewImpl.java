package com.fy.navi.scene.impl.search;

import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.search.ISceneMainSearchView;
import com.fy.navi.scene.ui.search.SceneMainSearchTopPartView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.StackManager;

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
