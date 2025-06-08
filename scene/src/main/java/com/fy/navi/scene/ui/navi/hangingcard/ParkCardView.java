package com.fy.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.databinding.NaviSceneNearProvideStationParkBinding;
import com.fy.navi.scene.util.HandCardType;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.view.SwipeDeleteLayout;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/18
 * Description: [悬挂卡-停车场]
 */
public class ParkCardView extends CardView<NaviSceneNearProvideStationParkBinding> {


    public ParkCardView(@NonNull Context context, OnCardChangeListener listener, List<PoiInfoEntity> list, HandCardType type) {
        super(context, listener, list, type);
    }

    @Override
    NaviSceneNearProvideStationParkBinding initViewBinding(Context context) {
        return NaviSceneNearProvideStationParkBinding.inflate(LayoutInflater.from(context), this, true);
    }

    @Override
    public SwipeDeleteLayout getSwipeView() {
        return mBinding.swipeDeleteView;
    }

    @Override
    public View getNaviView() {
        return mBinding.viewNaviNow;
    }

    @Override
    public void initListener() {
        super.initListener();
        mBinding.viewBg.setOnClickListener(v -> {
            showDetail(mType);
        });
    }

    @Override
    public void updateUi(List<PoiInfoEntity> dataList) {
        if (ConvertUtils.isEmpty(dataList)) {
            return;
        }
        final PoiInfoEntity poiInfo = dataList.get(0);
        mBinding.tvTitle.setText(poiInfo.getName());
        mBinding.tvTitleUnexpand.setText(poiInfo.getName());
        mBinding.tvDistance.setText(poiInfo.getDistance());
        List<ParkingInfo> parkingInfos = poiInfo.getParkingInfoList();
        if (!ConvertUtils.isEmpty(parkingInfos)) {
            final ParkingInfo parkingInfo = parkingInfos.get(0);
            mBinding.tvFreeSize.setVisibility(parkingInfo.getSpaceTotal() > 0 ? View.VISIBLE : View.GONE);
            mBinding.tvTotalSize.setVisibility(parkingInfo.getSpaceTotal() > 0 ? View.VISIBLE : View.GONE);
            mBinding.tvFreeSize.setText(String.valueOf(parkingInfo.getSpaceFree()));
            mBinding.tvTotalSize.setText("/" + parkingInfo.getSpaceTotal());
        }
        if(CardManager.getInstance().endIsService(poiInfo)){
            mBinding.tvSubTitle.setVisibility(VISIBLE);
            mBinding.tvSubTitleUnexpand.setVisibility(VISIBLE);
        } else {
            mBinding.tvSubTitle.setVisibility(GONE);
            mBinding.tvSubTitleUnexpand.setVisibility(GONE);
        }
    }

    @Override
    public void setExpandState(boolean isExpand){
        if(mBinding == null){
            return;
        }
        if(isExpand){
            mBinding.clParkExpand.setVisibility(View.VISIBLE);
            mBinding.clParkUnexpand.setVisibility(View.GONE);
        } else {
            mBinding.clParkExpand.setVisibility(View.GONE);
            mBinding.clParkUnexpand.setVisibility(View.VISIBLE);
        }
    }
}
