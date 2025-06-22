package com.sgm.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneSearchPoiList;
import com.sgm.navi.scene.ui.search.SceneSugSearchPoiList;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.StackManager;

import java.util.List;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class SceneSugSearchPoiListImpl extends BaseSceneModel<SceneSugSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;
    private int mTaskId;

    public int getMTaskId() {
        return mTaskId;
    }

    public SceneSugSearchPoiListImpl(final SceneSugSearchPoiList scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mScreenView.clearEditText();
    }

    /**
     * 预搜索
     * @param key 关键字
     */
    public void suggestionSearch(final String key) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "suggestionSearch  key:" + key);
        mTaskId = mSearchPackage.suggestionSearch(key);
    }

    /**
     * 终止搜索
     */
    public void abortSearch() {
        mSearchPackage.abortSearch();
    }

    /**
     * 终止搜索
     * @param taskId taskId
     */
    public void abortSearch(final int taskId) {
        mSearchPackage.abortSearch(taskId);
    }

    public List<History> getSearchKeywordRecord(){
        List<History> historyList = mSearchPackage.getSearchKeywordRecord();
        return historyList;
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mTaskId = 0;
    }
}
