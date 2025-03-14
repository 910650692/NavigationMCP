package com.fy.navi.hmi.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentHomeCompanyBinding;
import com.fy.navi.hmi.databinding.FragmentMapPointSearchBinding;
import com.fy.navi.scene.ui.favorite.SceneMapPointSearchView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * 地图选点
 */
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
        mBinding.mapPointSearchView.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        getSearchPoiInfo();
//        mBinding.homeCompanyView.setSearchListener(mViewModel);
//        mBinding.homeCompanyView.setOnItemClickListener(mViewModel);
//        mBinding.homeCompanyView.setActivity(mActivity);
    }

    private void getSearchPoiInfo() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
        int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE,-1);
        doSearch(poiInfoEntity);
        mBinding.mapPointSearchView.refreshPoiView(poiType);
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
//            mBinding.homeCompanyView.requestFocusAndShowKeyboard();
        }
    }

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mBinding.mapPointSearchView.onSearchResult(searchResultEntity);
//        mViewModel.notifySearchResult(searchResultEntity);
    }

    public void doSearch(PoiInfoEntity poiInfoEntity) {
        mBinding.mapPointSearchView.doSearch(poiInfoEntity);
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        FavoriteHelper.getInstance().setHomeCompanyType(-1);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }


    public static Bundle getBundle(int poiType,PoiInfoEntity poiInfoEntity){
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE,poiType);
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL,poiInfoEntity);
        return bundle;
    }
}
