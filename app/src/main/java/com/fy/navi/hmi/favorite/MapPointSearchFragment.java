package com.fy.navi.hmi.favorite;

import android.os.Bundle;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentMapPointSearchBinding;
import com.fy.navi.scene.api.search.ISceneTerminalParking;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;


public class MapPointSearchFragment extends BaseFragment<FragmentMapPointSearchBinding, MapPointSearchViewModel> {


    @Override
    public int onLayoutId() {
        return R.layout.fragment_map_point_search;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.mapPointSearchView.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        getSearchPoiInfo();
//        mBinding.homeCompanyView.setSearchListener(mViewModel);
//        mBinding.homeCompanyView.setOnItemClickListener(mViewModel);
//        mBinding.homeCompanyView.setActivity(mActivity);
    }

    /**
     * getSearchPoiInfo
     */
    private void getSearchPoiInfo() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
        final int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE,-1);
        doSearch(poiInfoEntity);
        mBinding.mapPointSearchView.refreshPoiView(poiType);
        mBinding.mapPointSearchView.setClickListener(new ISceneTerminalParking() {
            @Override
            public void closeSearch() {
                FavoriteHelper.getInstance().setHomeCompanyType(-1);
            }
        });
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
//            mBinding.homeCompanyView.requestFocusAndShowKeyboard();
        }
    }

    /**
     * notifySearchResult
     * @param taskId 任务id
     * @param searchResultEntity 数据回调实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mBinding.mapPointSearchView.onSearchResult(taskId, searchResultEntity);
    }

    /**
     * doSearch
     * @param poiInfoEntity
     */
    public void doSearch(final PoiInfoEntity poiInfoEntity) {
        mBinding.mapPointSearchView.doSearch(poiInfoEntity);
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }


    /**
     * getBundle
     * @param poiType
     * @param poiInfoEntity
     * @return bundle
     */
    public static Bundle getBundle(final int poiType, final PoiInfoEntity poiInfoEntity){
        final Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE,poiType);
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL,poiInfoEntity);
        return bundle;
    }
}
