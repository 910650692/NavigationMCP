package com.sgm.navi.scene.ui.navi.view;

import android.animation.ValueAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.TbtLoadingViewBinding;
import com.sgm.navi.ui.view.SkinConstraintLayout;

public class TbtLoadingView extends SkinConstraintLayout {

    private ValueAnimator mAnimator;
    private float mAngelTemp = 0;
    private TbtLoadingViewBinding mBinding;

    public TbtLoadingView(@NonNull Context context) {
        super(context);
        initView(context);
    }

    public TbtLoadingView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    public TbtLoadingView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView(context);
    }

    private void initView(Context context) {
        LayoutInflater inflater =
                (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View view = inflater.inflate(R.layout.tbt_loading_view, this);
        mBinding = TbtLoadingViewBinding.bind(view);
    }

    public void startLoading() {
        if (mAnimator == null || !mAnimator.isRunning()) {
            initLoadAnim(mBinding.ivLoading);
            mAnimator.start();
        }
    }

    public void stopLoading() {
        // 如果动画已存在并正在运行，则取消并清理
        if (mAnimator != null) {
            if (mAnimator.isRunning()) {
                mAnimator.cancel();
            }
            mAnimator.removeAllUpdateListeners(); // 清理旧监听器
            mAnimator = null;
        }
    }

    /**
     * 初始化加载动画
     * @param sivLoading 加载动画视图
     */
    private void initLoadAnim(final View sivLoading) {
        stopLoading();
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

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        stopLoading();
    }
}
