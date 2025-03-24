package com.fy.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchPoiList;
import com.fy.navi.scene.ui.search.SceneSugSearchPoiList;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class SceneSugSearchPoiListImpl extends BaseSceneModel<SceneSugSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;

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
        mSearchPackage.suggestionSearch(key);
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
}
