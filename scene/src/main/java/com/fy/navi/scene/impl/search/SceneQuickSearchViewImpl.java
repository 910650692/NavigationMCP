package com.fy.navi.scene.impl.search;

import android.os.Bundle;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneQuickSearchView;
import com.fy.navi.scene.ui.search.SceneQuickSearchView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneQuickSearchViewImpl extends BaseSceneModel<SceneQuickSearchView> implements ISceneQuickSearchView {
    private final SearchPackage mSearchPackage;
    public SceneQuickSearchViewImpl(final SceneQuickSearchView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeSearch(final int type) {
        if (type == AutoMapConstant.SearchType.AROUND_SEARCH) {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeAllFragment();
        } else {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        }
        mSearchPackage.clearLabelMark();
    }

    @Override
    public void closeSearchOpenFromNavi() {
        Bundle bundle = new Bundle();
        bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(bundle);
        mSearchPackage.clearLabelMark();
    }
}
