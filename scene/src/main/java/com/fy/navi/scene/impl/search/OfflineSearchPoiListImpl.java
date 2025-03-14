package com.fy.navi.scene.impl.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchPoiList;
import com.fy.navi.scene.ui.search.OfflineSearchPoiList;
import com.fy.navi.scene.ui.search.SceneSugSearchPoiList;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class OfflineSearchPoiListImpl extends BaseSceneModel<OfflineSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;

    public OfflineSearchPoiListImpl(OfflineSearchPoiList scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mScreenView.clearEditText();
    }

    public void keywordSearch(int pageNum, String keyword, int adCode) {
        Logger.d(SEARCH_HMI_TAG, "keywordSearch", keyword);
        mSearchPackage.keywordSearch(pageNum, keyword, adCode);
    }

    public void suggestionSearch(String key) {
        Logger.d(SEARCH_HMI_TAG, "suggestionSearch  key:" + key);
        int taskId = mSearchPackage.suggestionSearch(key);
    }

    public void abortSearch() {
        mSearchPackage.abortSearch();
    }

    public void abortSearch(int taskId) {
        mSearchPackage.abortSearch(taskId);
    }
}
