package com.fy.navi.hmi.poi.chargingstation;

import static com.android.utils.TimeUtils.formatTimeRange;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.fy.navi.service.define.search.CostTime;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

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

    public ArrayList<CostTime> getCostTimes(ArrayList<CostTime> costTimes){
        for (int i = 0; i < costTimes.size(); i++) {
            String time = costTimes.get(i).getTime();
            if(!ConvertUtils.isEmpty(time)){
                time = formatTimeRange(time);
            }
            costTimes.get(i).setmIsCurrentTime(TimeUtils.isCurrentTimeInRange(time));
        }
        return costTimes;
    }
}
