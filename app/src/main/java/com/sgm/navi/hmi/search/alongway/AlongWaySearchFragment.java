package com.sgm.navi.hmi.search.alongway;

import android.content.res.TypedArray;
import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentAlongWayBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 沿途搜索页面
 */
@Route(path = RoutePath.Search.ALONG_WAY_SEARCH_FRAGMENT)
public class AlongWaySearchFragment extends BaseFragment<FragmentAlongWayBinding, AlongWaySearchViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_along_way;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.sceneQuickSearchView.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        defaultDataProcessing();
        getBundleData();
    }

    /**
     * 获取bundle数据
     */
    private void getBundleData() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        final int isOpenFromNavi = parsedArgs.getInt(NaviConstant.NAVI_CONTROL, 0);
        if (isOpenFromNavi == 1) {
            mBinding.sceneQuickSearchView.setNaviControl(true);
        }
    }


    /**
     * 默认数据处理
     */
    private void defaultDataProcessing() {
        final int powerType = mViewModel.powerType();
        final String[] categories;
        final TypedArray iconArray;
        // 油车
        if (powerType == 0) {
            categories = getResources().getStringArray(R.array.along_way_categories_gas_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_gas_icons);
        } else if (powerType == 1) {
            categories = getResources().getStringArray(R.array.along_way_categories_charging_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_charging_icons);
        } else if (powerType == 2) {
            categories = getResources().getStringArray(R.array.along_way_categories_phev_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_phev_icons);
        } else {
            categories = getResources().getStringArray(R.array.along_way_categories_gas_name);
            iconArray = getResources().obtainTypedArray(R.array.along_way_category_gas_icons);
        }
        mBinding.sceneQuickSearchView.setQuickSearchListAdapterData(iconArray, categories);
        mBinding.sceneQuickSearchView.setSearchType(AutoMapConstant.SearchType.ALONG_WAY_SEARCH);
        mBinding.sceneQuickSearchView.setPoiInfoEntity(null);
        mBinding.sceneQuickSearchView.setTextView(getString(R.string.along_way_search_hint));
    }

    /**
     * 搜索结果回调
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            mBinding.sceneQuickSearchView.notifySearchResult(taskId, searchResultEntity);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
