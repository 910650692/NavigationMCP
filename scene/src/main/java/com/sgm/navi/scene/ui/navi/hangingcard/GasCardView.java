package com.sgm.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.databinding.NaviSceneNearProvideStationGasBinding;
import com.sgm.navi.scene.util.HandCardType;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.view.SwipeDeleteLayout;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/18
 * Description: [悬挂卡-加油站]
 */
public class GasCardView extends CardView<NaviSceneNearProvideStationGasBinding> {


    public GasCardView(@NonNull Context context, OnCardChangeListener listener, List<PoiInfoEntity> list, HandCardType type) {
        super(context, listener, list, type);
    }

    @Override
    NaviSceneNearProvideStationGasBinding initViewBinding(Context context) {
        return NaviSceneNearProvideStationGasBinding.inflate(LayoutInflater.from(context), this, true);
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
        mPoiInfo = poiInfo;
        final List<GasStationInfo> gasInfos = poiInfo.getStationList();
        mBinding.tvTitle.setText(poiInfo.getName());
        mBinding.tvTitleUnexpand.setText(poiInfo.getName());
        mBinding.tvDistance.setText(poiInfo.getDistance());
        mBinding.llTypeAndPrice.setVisibility(ConvertUtils.isEmpty(gasInfos) ? View.GONE : View.VISIBLE);
        if (!ConvertUtils.isEmpty(gasInfos)) {
            setGasTypeAndPrice(poiInfo.getStationList());
        }
        if(CardManager.getInstance().endIsService(poiInfo)){
            mBinding.tvSubTitle.setVisibility(VISIBLE);
            mBinding.tvSubTitleUnexpand.setVisibility(VISIBLE);
        } else {
            mBinding.tvSubTitle.setVisibility(GONE);
            mBinding.tvSubTitleUnexpand.setVisibility(GONE);
        }
    }

    private void setGasTypeAndPrice(final List<GasStationInfo> infos) {
        if (ConvertUtils.isEmpty(infos)) {
            mBinding.llTypeAndPrice.setVisibility(ViewGroup.GONE);
        } else if (infos.size() >= 2) {
            mBinding.llTypeAndPrice.setVisibility(ViewGroup.VISIBLE);
            final GasStationInfo firstBean = infos.get(0);
            final GasStationInfo secondBean = infos.get(1);
            mBinding.tv95.setVisibility(View.VISIBLE);
            mBinding.tv95.setText(firstBean.getType());
            mBinding.tv95Price.setText(firstBean.getPrice());

            mBinding.tv92.setVisibility(View.VISIBLE);
            mBinding.tv92.setText(secondBean.getType());
            mBinding.tv92Price.setText(secondBean.getPrice());
        } else if (infos.size() == 1) {
            final GasStationInfo firstBean = infos.get(0);
            mBinding.tv95.setVisibility(View.VISIBLE);
            mBinding.tv95.setText(firstBean.getType());
            mBinding.tv95Price.setText(firstBean.getPrice());

            mBinding.tv92.setVisibility(View.GONE);
        }
    }

    @Override
    public void setExpandState(boolean isExpand){
        if(mBinding == null){
            return;
        }
        if(isExpand){
            mBinding.clGasExpand.setVisibility(View.VISIBLE);
            mBinding.clGasUnexpand.setVisibility(View.GONE);
        } else {
            mBinding.clGasExpand.setVisibility(View.GONE);
            mBinding.clGasUnexpand.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void updateDistance(String distance) {
        Logger.i(TAG, "distance: ", distance);
        if (ConvertUtils.isNull(mBinding)) {
            Logger.i(TAG, "binding is null");
            return;
        }
        mBinding.tvDistance.setText(distance);
    }
}
