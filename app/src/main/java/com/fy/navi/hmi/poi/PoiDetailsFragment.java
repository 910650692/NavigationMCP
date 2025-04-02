package com.fy.navi.hmi.poi;


import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentPoiDetailsBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author lvww
 * @version \$Revision1.0\$
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
        mBinding.scenePoiDetailContentView.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        getSearchPoiInfo();
    }

    @Override
    protected void onNewIntent(final Bundle bundle) {
        super.onNewIntent(bundle);
    }

    /**
     * 获取搜索poi信息
     */
    private void getSearchPoiInfo() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

//        String sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
        final int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        mBinding.scenePoiDetailContentView.refreshPoiView(poiType);
        mBinding.scenePoiDetailContentView.doSearch(poiInfoEntity);
    }

    /**
     * 搜索结果回调
     *
     * @param searchResultEntity 搜索结果实体类
     */
    public void onSearchResult(final SearchResultEntity searchResultEntity) {
        mBinding.scenePoiDetailContentView.onSearchResult(searchResultEntity);
    }

    @Override
    protected PoiDetailsViewModel initViewModel() {
        mViewModel = new PoiDetailsViewModel(AppContext.getInstance().getMApplication());
        return mViewModel;
    }
}
