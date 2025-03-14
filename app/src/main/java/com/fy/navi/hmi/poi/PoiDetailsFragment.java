package com.fy.navi.hmi.poi;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentPoiDetailsBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
@Route(path = RoutePath.Search.POI_DETAILS_FRAGMENT)
public class PoiDetailsFragment extends BaseFragment<FragmentPoiDetailsBinding, PoiDetailsViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_poi_details;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.scenePoiDetailContentView.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        getSearchPoiInfo();
    }

    @Override
    protected void onNewIntent(Bundle bundle) {
        super.onNewIntent(bundle);
    }

    private void getSearchPoiInfo() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        String sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
        int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        mBinding.scenePoiDetailContentView.refreshPoiView(poiType);
        mBinding.scenePoiDetailContentView.doSearch(poiInfoEntity);
    }

    public void onSearchResult(SearchResultEntity searchResultEntity) {
        mBinding.scenePoiDetailContentView.onSearchResult(searchResultEntity);
    }
}
