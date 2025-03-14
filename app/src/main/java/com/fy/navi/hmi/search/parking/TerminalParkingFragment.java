package com.fy.navi.hmi.search.parking;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentTerminalParkingBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @Author: baipeng0904
 * @Description: TerminalParkingFragment
 * @CreateDate: 2025/3/5 13:49
 */
public class TerminalParkingFragment extends BaseFragment<FragmentTerminalParkingBinding, TerminalParkingViewModel> {

    @Override
    public int onLayoutId() {
        Logger.d(SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.fragment_terminal_parking;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(SEARCH_HMI_TAG, "onInitView");
        mBinding.sceneTerminalParkingListView.setScreenId(MapTypeId.valueOf(mScreenId));
    }


    @Override
    public void onInitData() {
        Logger.d(SEARCH_HMI_TAG, "onInitData");
        getBundleData();
    }

    private void getBundleData() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        GeoPoint geoPoint = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE);
        mBinding.sceneTerminalParkingListView.aroundSearch(geoPoint);
    }

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
        mBinding.sceneTerminalParkingListView.notifySearchResult(searchResultEntity);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
