package com.fy.navi.hmi.search.parking;


import android.os.Bundle;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentTerminalParkingBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 */
public class TerminalParkingFragment extends BaseFragment<FragmentTerminalParkingBinding, TerminalParkingViewModel> {

    @Override
    public int onLayoutId() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.fragment_terminal_parking;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitView");
        mBinding.sceneTerminalParkingListView.setScreenId(MapType.valueOf(mScreenId));
    }


    @Override
    public void onInitData() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitData");
    }

    @Override
    public void onGetFragmentData() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onGetFragmentData");
        super.onGetFragmentData();
        getBundleData();
    }

    @Override
    public void onReStoreFragment() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment");
        super.onReStoreFragment();
        mViewModel.onReStoreFragment();
    }

    /**
     * 获取Bundle携带的数据
     */
    private void getBundleData() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final GeoPoint geoPoint = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE);
        mBinding.sceneTerminalParkingListView.aroundSearch(geoPoint);
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity) {
        mBinding.sceneTerminalParkingListView.notifySearchResult(searchResultEntity);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 终点停车场扎标点击事件
     * @param index 点击下标
     */
    public void onMarkTerminalParkClickCallBack(final int index) {
        mBinding.sceneTerminalParkingListView.onMarkTerminalParkClickCallBack(index);
    }
}
