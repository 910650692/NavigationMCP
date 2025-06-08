package com.fy.navi.scene.ui.navi.hangingcard;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.NaviSceneNearProvideStationChargeBinding;
import com.fy.navi.scene.util.HandCardType;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.view.SwipeDeleteLayout;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/18
 * Description: [悬挂卡-充电站]
 */
public class ChargeCardView extends CardView<NaviSceneNearProvideStationChargeBinding> {


    public ChargeCardView(@NonNull Context context, OnCardChangeListener listener, List<PoiInfoEntity> list, HandCardType type) {
        super(context, listener, list, type);
    }

    @Override
    NaviSceneNearProvideStationChargeBinding initViewBinding(Context context) {
        return NaviSceneNearProvideStationChargeBinding.inflate(LayoutInflater.from(context), this, true);
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
        final boolean isEmpty = ConvertUtils.isEmpty(dataList);
        if (isEmpty || ConvertUtils.isNull(mBinding)) {
            Logger.i(TAG, "list is empty or binding is ");
            return;
        }
        final PoiInfoEntity poiInfo = dataList.get(0);
        final List<ChargeInfo> chargeInfos = poiInfo.getChargeInfoList();
        mBinding.tvTitle.setText(poiInfo.getName());
        mBinding.tvTitleUnexpand.setText(poiInfo.getName());
        mBinding.tvDistance.setText(poiInfo.getDistance());
        if (!ConvertUtils.isEmpty(chargeInfos)) {
            final ChargeInfo chargeInfo = chargeInfos.get(0);
            setStationInfo(chargeInfo);
        }
        if(CardManager.getInstance().endIsService(poiInfo)){
            mBinding.tvSubTitle.setVisibility(VISIBLE);
            mBinding.tvSubTitleUnexpand.setVisibility(VISIBLE);
        } else {
            mBinding.tvSubTitle.setVisibility(GONE);
            mBinding.tvSubTitleUnexpand.setVisibility(GONE);
        }
        if(CardManager.getInstance().judgeDestinationIsChargeStation(poiInfo)){
            mBinding.layoutChargeDesc.tvAddress.setVisibility(VISIBLE);
            mBinding.layoutChargeDesc.tvAddress.setText(poiInfo.getAddress());
        } else {
            mBinding.layoutChargeDesc.tvAddress.setVisibility(GONE);
        }
    }

    private void setStationInfo(ChargeInfo chargeInfo) {
        if (ConvertUtils.isNull(chargeInfo)) return;
        final boolean haQuickStation = chargeInfo.getFast_total() > 0;
        final boolean hasLowerSlowStation = chargeInfo.getSlow_total() > 0;
        mBinding.layoutChargeDesc.root.setVisibility((!hasLowerSlowStation && !haQuickStation) ? View.GONE : View.VISIBLE);
        if (haQuickStation) {
            mBinding.layoutChargeDesc.root.setVisibility(View.VISIBLE);
            mBinding.layoutChargeDesc.tvQuickUse.setText(String.valueOf(chargeInfo.getFast_free()));
            mBinding.layoutChargeDesc.tvQuickTotal.setText("/" + chargeInfo.getFast_total());
            mBinding.layoutChargeDesc.tvQuickUse.setVisibility(View.VISIBLE);
            mBinding.layoutChargeDesc.tvQuickTotal.setVisibility(View.VISIBLE);
        } else {
            mBinding.layoutChargeDesc.tvQuickUse.setVisibility(View.GONE);
            mBinding.layoutChargeDesc.tvQuickTotal.setVisibility(View.GONE);
        }

        if (hasLowerSlowStation) {
            mBinding.layoutChargeDesc.root.setVisibility(View.VISIBLE);
            mBinding.layoutChargeDesc.tvSlowUse.setText(String.valueOf(chargeInfo.getSlow_free()));
            mBinding.layoutChargeDesc.tvSlowTotal.setText("/" + chargeInfo.getSlow_total());
            mBinding.layoutChargeDesc.tvSlowUse.setVisibility(View.VISIBLE);
            mBinding.layoutChargeDesc.tvSlowTotal.setVisibility(View.VISIBLE);
        } else {
            mBinding.layoutChargeDesc.tvSlowUse.setVisibility(View.GONE);
            mBinding.layoutChargeDesc.tvSlowTotal.setVisibility(View.GONE);
        }

        if (CardManager.getInstance().isPlentiful(chargeInfo)) {
            mBinding.layoutChargeDesc.tvTense.setVisibility(View.VISIBLE);
        } else {
            mBinding.layoutChargeDesc.tvTense.setVisibility(View.GONE);
        }
    }

    @Override
    public void setExpandState(boolean isExpand){
        if(mBinding == null){
            return;
        }
        Logger.i(TAG, " setExpandState isExpand:"+isExpand);
        if(isExpand){
            mBinding.clChargeExpand.setVisibility(View.VISIBLE);
            mBinding.clChargeUnexpand.setVisibility(View.GONE);
        } else {
            mBinding.clChargeExpand.setVisibility(View.GONE);
            mBinding.clChargeUnexpand.setVisibility(View.VISIBLE);
        }
    }
}
