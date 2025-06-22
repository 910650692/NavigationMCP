package com.sgm.navi.scene.impl.search;

import android.os.Bundle;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.search.ISceneQuickSearchView;
import com.sgm.navi.scene.ui.search.SceneQuickSearchView;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.ui.base.StackManager;

import java.util.ArrayList;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 继承自BaseSceneModel，并封装了与搜索相关的操作，如关闭搜索页面、中止搜索等。
 */
public class SceneQuickSearchViewImpl extends BaseSceneModel<SceneQuickSearchView> implements ISceneQuickSearchView {
    private final SearchPackage mSearchPackage;
    private int mTaskId;

    /**
     * 获取高德SDK请求任务Id
     * @return 请求任务Id
     */
    public int getMTaskId() {
        return mTaskId;
    }
    public SceneQuickSearchViewImpl(final SceneQuickSearchView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void closeSearch(final int type) {
        if (type == AutoMapConstant.SearchType.AROUND_SEARCH) {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeAllFragmentsUntilTargetFragment("AroundSearchFragment");
        } else {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        }
        mSearchPackage.clearLabelMark();
    }

    @Override
    public void closeSearchOpenFromNavi() {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                Bundle bundle = new Bundle();
                bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
                StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(bundle);
            }
        });
        mSearchPackage.clearLabelMark();
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
     * 清除poi扎标
     * @param poiInfo 清除扎标对象
     */
    public void createPoiMarker(final PoiInfoEntity poiInfo){
        mSearchPackage.clearLabelMark();
        final ArrayList<PoiInfoEntity> arrayList = new ArrayList<>();
        arrayList.add(poiInfo);
        final SearchResultEntity searchResultEntity = new SearchResultEntity().setPoiList(arrayList);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"createLabelMarker: ");
        mSearchPackage.createLabelMarker(searchResultEntity);
    }
}
