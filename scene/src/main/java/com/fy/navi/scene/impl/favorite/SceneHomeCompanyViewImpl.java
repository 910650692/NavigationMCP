package com.fy.navi.scene.impl.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.favorite.ISceneHomeCompanyView;
import com.fy.navi.scene.ui.favorite.SceneHomeCompanyView;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @Author: qlzou
 * @Description: 家和公司
 * @CreateDate: $ $
 */
public class SceneHomeCompanyViewImpl extends BaseSceneModel<SceneHomeCompanyView> implements ISceneHomeCompanyView {
    private final SearchPackage mSearchPackage;
    public SceneHomeCompanyViewImpl(SceneHomeCompanyView scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    @Override
    public void onClickQuickSearch(int position) {
        mScreenView.onClickQuickSearch(position);
    }

    @Override
    public void onClickEditText() {
        mScreenView.onClickEditText();
    }

    public void suggestionSearch(String key) {
        Logger.d(SEARCH_HMI_TAG, "suggestionSearch  key:" + key);
        int taskId = mSearchPackage.suggestionSearch(key);
    }

}
