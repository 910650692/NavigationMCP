package com.fy.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.search.ISceneSearchPoiList;
import com.fy.navi.scene.ui.search.OfflineSearchPoiList;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class OfflineSearchPoiListImpl extends BaseSceneModel<OfflineSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;

    public OfflineSearchPoiListImpl(final OfflineSearchPoiList scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeSearch() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        mScreenView.clearEditText();
    }

    /**
     * 关键字搜索
     * @param pageNum 页数
     * @param keyword 关键字
     * @param adCode 城市编码
     * @param isSilent 是否静默搜
     */
    public void keywordSearch(final int pageNum, final String keyword, final int adCode, final boolean isSilent) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "keywordSearch", keyword);
        mSearchPackage.keywordSearch(pageNum, keyword, adCode, isSilent);
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
     * 中止搜索
     */
    public void abortSearch() {
        mSearchPackage.abortSearch();
    }

    /**
     * 中止搜索
     * @param taskId 任务id
     */
    public void abortSearch(final int taskId) {
        mSearchPackage.abortSearch(taskId);
    }
}
