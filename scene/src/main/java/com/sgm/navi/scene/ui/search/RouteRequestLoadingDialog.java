package com.sgm.navi.scene.ui.search;

import android.animation.ValueAnimator;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.animation.LinearInterpolator;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.LayoutSearchLoadingBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;

/**
 * @author wentengsong
 * @version \$Revision1.0\$
 * @Description: 路线请求加载框
 * @CreateDate: $ $
 */
public class RouteRequestLoadingDialog extends BaseFullScreenDialog<LayoutSearchLoadingBinding> {
    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;
    private OnCloseClickListener mListener;

    public RouteRequestLoadingDialog(final Context context) {
        super(context);
    }

    @Override
    protected LayoutSearchLoadingBinding initLayout() {
        return LayoutSearchLoadingBinding.inflate(getLayoutInflater());
    }

    public void setOnCloseClickListener(OnCloseClickListener listener) {
        this.mListener = listener;
    }

    /**
     * 显示离线算路文言
     */
    public void showOfflineRouting() {
        mViewBinding.tvMessage.setText(ResourceUtils.Companion.getInstance().getText(R.string.route_offline_loading));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initLoadAnim(mViewBinding.ivLoading);
        mViewBinding.tvMessage.setText(ResourceUtils.Companion.getInstance().getText(R.string.route_search_loading));
        mViewBinding.ivClose.setOnClickListener(v -> {
            dismiss();
            if (mListener != null) {
                mListener.onClose();
            }
        });
    }

    /**
     * 初始化加载动画
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        // 如果动画已存在并正在运行，则取消并清理
        if (mAnimator != null) {
            if (mAnimator.isRunning()) {
                mAnimator.cancel();
            }
            mAnimator.removeAllUpdateListeners(); // 清理旧监听器
            mAnimator = null;
        }

        // 创建属性动画，从 0 到 360 度循环旋转
        mAnimator = ValueAnimator.ofFloat(0f, 360f);
        mAnimator.setDuration(2000); // 动画持续时间
        mAnimator.setRepeatCount(ValueAnimator.INFINITE); // 无限重复
        mAnimator.setInterpolator(new LinearInterpolator()); // 线性插值器

        // 使用局部变量提升访问速度
        final float[] lastAngle = {mAngelTemp};

        // 添加动画更新监听器
        mAnimator.addUpdateListener(animation -> {
            final float angles = (float) animation.getAnimatedValue();
            int angle = (int) angles;
            if (shouldSkipUpdate(angle, lastAngle[0])) {
                return;
            }
            sivLoading.setRotation(angle);
            lastAngle[0] = angle; // 更新临时角度值
        });
    }

    /**
     * 用于控制角度变化频率的辅助方法
     * @param angle 当前角度
     * @param lastAngle 上次角度
     * @return 是否跳过更新
     */
    private boolean shouldSkipUpdate(final int angle, final float lastAngle) {
        final int changeAngle = angle - (int) lastAngle;
        final int angleStep = 30;
        return changeAngle > 0 && changeAngle <= angleStep;
    }

    /**
     * 停止动画
     */
    private void stopAnimator() {
        if (mAnimator == null) {
            return;
        }
        if (mAnimator.isRunning()) {
            mAnimator.cancel();
        }
        mAnimator.removeAllUpdateListeners(); // 释放引用
        mAnimator = null;
        mAngelTemp = 0;
    }

    @Override
    public void show() {
        super.show();
        if (mAnimator != null && !mAnimator.isRunning()) {
            mAnimator.start();
        }
    }

    @Override
    public void hide() {
        super.hide();
        stopAnimator();
    }

    @Override
    public void dismiss() {
        super.dismiss();
        stopAnimator();
    }

    public interface OnCloseClickListener{
        void onClose();
    }
}
