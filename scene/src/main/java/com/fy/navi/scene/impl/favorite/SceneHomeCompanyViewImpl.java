package com.fy.navi.scene.impl.favorite;


import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.favorite.ISceneHomeCompanyView;
import com.fy.navi.scene.ui.favorite.SceneHomeCompanyView;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @author qlzou
 * @version \$Revision1.0\$
 * @Description: 家和公司
 * @CreateDate: $ $
 */
public class SceneHomeCompanyViewImpl extends BaseSceneModel<SceneHomeCompanyView> implements ISceneHomeCompanyView {
    private final SearchPackage mSearchPackage;
    public SceneHomeCompanyViewImpl(final SceneHomeCompanyView scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
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
    public void onClickEditText() {
        mScreenView.onClickEditText();
    }

    /**
     * 预搜索
     * @param key 关键字
     */
    public void suggestionSearch(final String key) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "suggestionSearch  key:" + key);
        mSearchPackage.suggestionSearch(key);
    }

}
