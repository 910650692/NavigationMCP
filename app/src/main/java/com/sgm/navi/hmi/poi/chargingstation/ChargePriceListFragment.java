package com.sgm.navi.hmi.poi.chargingstation;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentChargePriceListBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.CostTime;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;

@Route(path = RoutePath.Search.POI_CHARGE_PRICE_ALL_DAY_FRAGMENT)
public class ChargePriceListFragment extends BaseFragment<FragmentChargePriceListBinding, BaseChargePriceListViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_charge_price_list;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public int onFragmentId() {
        return super.onFragmentId();
    }

    @Override
    public void onInitView() {
        mBinding.chargePrice.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onInitData() {
        getPriceListData();
    }

    private void getPriceListData(){
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_CHARGE_PRICE_LIST);
        if(!ConvertUtils.isNull(poiInfoEntity)
                && !ConvertUtils.isEmpty(poiInfoEntity.getChargeInfoList())
                && !ConvertUtils.isEmpty(poiInfoEntity.getChargeInfoList().get(0))){
            ArrayList<CostTime> costTimes = mViewModel.getCostTimes(poiInfoEntity.getChargeInfoList().get(0).getCostItem());
            mBinding.chargePrice.notifyChargePriceList(costTimes);
        }else{
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"costItem is empty");
        }
    }
}