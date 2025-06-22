package com.sgm.navi.scene.impl.search;


import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneSearchPoiList;
import com.sgm.navi.scene.ui.search.OfflineSearchPoiList;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.sgm.navi.ui.base.StackManager;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class OfflineSearchPoiListImpl extends BaseSceneModel<OfflineSearchPoiList> implements ISceneSearchPoiList {
    private final SearchPackage mSearchPackage;
    private UserTrackPackage mUserTrackPackage;
    private int mTaskId;
    public int getMTaskId() {
        return mTaskId;
    }
    public OfflineSearchPoiListImpl(final OfflineSearchPoiList scrollView) {
        super(scrollView);
        mSearchPackage = SearchPackage.getInstance();
        mUserTrackPackage = UserTrackPackage.getInstance();
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
        mTaskId = mSearchPackage.keywordSearch(pageNum, keyword, adCode, isSilent, false);
        SearchHistoryItemBean item = new SearchHistoryItemBean();
        item.setName(keyword);
        item.setUpdateTime(System.currentTimeMillis());
        mUserTrackPackage.addSearchHistory(item);
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
