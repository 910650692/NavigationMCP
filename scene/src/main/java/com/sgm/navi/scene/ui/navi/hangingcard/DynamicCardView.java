package com.sgm.navi.scene.ui.navi.hangingcard;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.HangingCardDynamicLayoutBinding;
import com.sgm.navi.scene.util.HandCardType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/18
 * Description: [悬挂卡-充电站]
 */
public class DynamicCardView extends CardView<HangingCardDynamicLayoutBinding> {
    private float mDownY;
    private float threaHoldDis = 20;

    public DynamicCardView(@NonNull Context context, OnCardChangeListener listener, List<PoiInfoEntity> list, HandCardType type) {
        super(context, listener, list, type);
    }


    @Override
    HangingCardDynamicLayoutBinding initViewBinding(Context context) {
        return HangingCardDynamicLayoutBinding.inflate(LayoutInflater.from(context), this, true);
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public void initListener() {
        super.initListener();
        setOnTouchListener((v, event) -> {
            switch (event.getAction()) {
                case MotionEvent.ACTION_DOWN -> {
                    mDownY = event.getY();
                }
                case MotionEvent.ACTION_UP -> {
                    final float moveY = event.getY() - mDownY;
                    if (moveY > threaHoldDis) {
                        expandItem();
                    }
                }
                default -> {
                }
            }
            return true;
        });
        mBinding.scRoot.setOnClickListener(v -> {
            showDetail(mType);
        });
    }

    /***
     * 展开列表
     */
    private void expandItem() {
        if (!ConvertUtils.isNull(mListener)) {
            mListener.expandAll();
        }
    }

    @Override
    public void updateUi(List<PoiInfoEntity> dataList) {
        final boolean isEmpty = ConvertUtils.isEmpty(dataList);
        if (isEmpty) {
            return;
        }
        final PoiInfoEntity poiInfo = dataList.get(0);
        mBinding.tvName.setText(poiInfo.getName());
        if (!ConvertUtils.isEmpty(poiInfo.getChargeInfoList())) {
            mBinding.ivIcon.setImageResource(R.drawable.img_electricity_low_42);
        } else if (!ConvertUtils.isEmpty(poiInfo.getStationList())) {
            mBinding.ivIcon.setImageResource(R.drawable.img_refuel_low_58);
        } else if (!ConvertUtils.isEmpty(poiInfo.getParkingInfoList())) {
            mBinding.ivIcon.setImageResource(R.drawable.img_navi_parking);
        } else {
            Logger.e(TAG, "未知的悬挂卡类型！");
        }
    }

}
