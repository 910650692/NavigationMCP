package com.fy.navi.scene.impl.navi.search;

import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.ISceneNaviSearchView;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.search.SceneNaviSearchView;
import com.fy.navi.scene.ui.search.SearchLoadingDialog;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

public class SceneNaviSearchViewImpl extends BaseSceneModel<SceneNaviSearchView> implements
        SearchResultCallback, ISceneNaviSearchView {
    public static final String TAG = "SceneNaviSearchViewImpl";

    private ISceneCallback mISceneCallback;

    public ObservableField<Boolean> mNaviAlongWayListViewVisible;

    public ObservableField<Boolean> mNaviSearchResultViewVisible;

    private SearchLoadingDialog mSearchLoadingDialog;

    private int mSearchTaskId;

    public SceneNaviSearchViewImpl(SceneNaviSearchView mScreenView) {
        super(mScreenView);
        mNaviAlongWayListViewVisible = new ObservableField<>(false);
        mNaviSearchResultViewVisible = new ObservableField<>(false);
        mSearchLoadingDialog = new SearchLoadingDialog(mScreenView.getContext());
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        OpenApiHelper.registerSearchResultCallback(TAG, this);
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        OpenApiHelper.unRegisterSearchResultCallback(TAG);
        if (null != mSearchLoadingDialog) {
            mSearchLoadingDialog.hide();
            mSearchLoadingDialog = null;
        }
    }

    public int powerType() {
        return OpenApiHelper.powerType();
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message,
                               SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSearchResult taskId:" + taskId + " errorCode:" + errorCode +
                " message:" + message);
        if (taskId == mSearchTaskId) {
            mSearchLoadingDialog.hide();
            mScreenView.setSearchResultData(searchResultEntity);
        }
    }

    public void goSearchView(String keyWord, int searchType) {
        startSearch(keyWord, searchType);
        mNaviSearchResultViewVisible.set(true);
        mNaviAlongWayListViewVisible.set(false);
        OpenApiHelper.enterPreview(mMapTypeId);
    }

    public void goAlongWayList() {
        mNaviSearchResultViewVisible.set(false);
        mNaviAlongWayListViewVisible.set(true);
    }

    @Override
    public void closeSearchView() {
        mISceneCallback.closeSearchView();
        OpenApiHelper.exitPreview(mMapTypeId);
        OpenApiHelper.clearLabelMark();
    }

    @Override
    public void closeSearchResultView() {
        mScreenView.closeSearchResultView();
        mISceneCallback.closeSearchView();
    }

    @Override
    public void startSearch(String keyword, int searchType) {
        mSearchTaskId = OpenApiHelper.getSearchResult(keyword, searchType);
        mSearchLoadingDialog.show();
    }

    @Override
    public void goSearchResultView(String keyWord, int searchType) {
        goSearchView(keyWord, searchType);
    }

    public void addISceneCallback(ISceneCallback iSceneCallback) {
        mISceneCallback = iSceneCallback;
    }
}
