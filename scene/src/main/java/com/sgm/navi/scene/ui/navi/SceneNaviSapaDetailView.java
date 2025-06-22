package com.sgm.navi.scene.ui.navi;


import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviSapaDetailViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviSapaDetailImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.ui.view.SkinImageView;
import com.sgm.navi.ui.view.SkinTextView;

public class SceneNaviSapaDetailView extends NaviSceneBase<SceneNaviSapaDetailViewBinding,
        SceneNaviSapaDetailImpl> {

    public SceneNaviSapaDetailView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviSapaDetailView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSapaDetailView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs,
                                   final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SAPA_DETAIL_INFO;
    }

    @Override
    protected SceneNaviSapaDetailViewBinding createViewBinding(final LayoutInflater inflater,
                                                               final ViewGroup viewGroup) {
        return SceneNaviSapaDetailViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSapaDetailImpl initSceneImpl() {
        return new SceneNaviSapaDetailImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSapaDetail(mScreenViewModel);
        mViewBinding.sapaServiceDetail.setSapaDetail(mScreenViewModel);
        mViewBinding.sapaTollDetail.setSapaDetail(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    /**
     * @param type 场景类型
     * @param sapaInfoEntity 导航信息
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        mScreenViewModel.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }


    /**
     * 更新服务区详情的剩余电量
     * @param leftCharge 剩余电量
     */
    public void updateServiceChargeUi(int leftCharge) {
        mViewBinding.sapaServiceDetail.sivServiceRemainChargeIcon.setVisibility(VISIBLE);
        mViewBinding.sapaServiceDetail.sivServiceDetailsRemiancharge.setVisibility(VISIBLE);
        setChargeUi(leftCharge, mViewBinding.sapaServiceDetail.sivServiceRemainChargeIcon,
                mViewBinding.sapaServiceDetail.sivServiceDetailsRemiancharge);
    }

    /**
     * 更新收费站详情的剩余电量
     * @param leftCharge 剩余电量
     */
    public void updateTollChargeUi(int leftCharge) {
        mViewBinding.sapaTollDetail.sivTollRemainChargeIcon.setVisibility(VISIBLE);
        mViewBinding.sapaTollDetail.sivTollDetailsRemiancharge.setVisibility(VISIBLE);
        setChargeUi(leftCharge, mViewBinding.sapaTollDetail.sivTollRemainChargeIcon,
                mViewBinding.sapaTollDetail.sivTollDetailsRemiancharge);
    }

    @SuppressLint("SetTextI18n")
    private void setChargeUi(final int chargeLeft, final SkinImageView img,
                             final SkinTextView text) {
        final int leftCharge = Math.max(-99, chargeLeft);
        if (!ConvertUtils.isEmpty(leftCharge)) {
            //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
            //0-20电量，显示低电量图片，文本变红
            //小于0%电量，显示空电量图片，文本变红
            if (leftCharge >= 50 && leftCharge <= 100) {
                img.setImageResource(R.drawable.img_electricity_full_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.poi_details_bottom_ff_00));
            } else if (leftCharge > 20 && leftCharge < 50) {
                img.setImageResource(R.drawable.img_electricity_medium_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.poi_details_bottom_ff_00));
            } else if (leftCharge > 0 && leftCharge <= 20) {
                img.setImageResource(R.drawable.img_electricity_low_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.navi_color_C73333_100));
            } else if (leftCharge <= 0) {
                img.setImageResource(R.drawable.img_electricity_empty_42);
                text.setTextColor(
                        ResourceUtils.Companion.getInstance().
                                getColor(R.color.navi_color_C73333_100));
            }
            text.setText(getContext().getString(R.string.remain_charge, leftCharge));
        }
    }
}
