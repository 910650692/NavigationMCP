package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseChargePriceListViewModel extends BaseViewModel<ChargePriceListFragment,ChargePriceListModel> {
    public BaseChargePriceListViewModel(@NonNull Application application) {
        super(application);
    }

    private final Action mRootClick = () -> {
    };

    public Action getRootClick() {
        return mRootClick;
    }

    @Override
    protected ChargePriceListModel initModel() {
        return new ChargePriceListModel();
    }
}
