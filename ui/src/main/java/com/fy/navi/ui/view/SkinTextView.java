package com.fy.navi.ui.view;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.util.AttributeSet;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatTextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.ui.R;

public class SkinTextView extends AppCompatTextView {
    private final Context mContext;
    private int mDefaultTextColor = 0;
    private int mSelectTextColor = 0;
    private Drawable mDefaultBgColor;
    private Drawable mSelectBgColor;
    /* 是否开启水波动效 */
    private boolean mForeground = false;
    private boolean mStartMarquee = false;

    public SkinTextView(final Context context) {
        this(context, null);
    }

    public SkinTextView(final Context context, @Nullable final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinTextView(final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.mContext = context;
        loadTextAttrs(context, attrs);
    }

    /**
     * 加载属性
     *
     * @param context 上下文
     * @param attrs   属性
     */
    private void loadTextAttrs(final Context context, @Nullable final AttributeSet attrs) {
        final TypedArray typedArray = context.obtainStyledAttributes(attrs, R.styleable.SkinTextView);
        mSelectTextColor = typedArray.getColor(R.styleable.SkinTextView_select_text_color, getCurrentTextColor());
        mSelectBgColor = typedArray.getDrawable(R.styleable.SkinTextView_select_background_color);
        mForeground = typedArray.getBoolean(R.styleable.SkinTextView_enable_foreground, false);
        mStartMarquee = typedArray.getBoolean(R.styleable.SkinTextView_marquee, false);
        mDefaultTextColor = getCurrentTextColor();
        mDefaultBgColor = getBackground();
        typedArray.recycle();
        initView();
    }

    /**
     * 初始化View
     */
    private void initView() {
        final TypedArray typedArray = mContext.getTheme().obtainStyledAttributes(new int[]{android.R.attr.selectableItemBackground});
        final int attr = typedArray.getResourceId(0, 0);
        typedArray.recycle();
        if (mForeground) {
            setForeground(mContext.getDrawable(attr));
        }
        if (mStartMarquee) {
            startTextViewMarquee();
        }
    }

    @Override
    public void setSelected(final boolean selected) {
        super.setSelected(selected);
        if (selected) {
            if (mSelectTextColor != mDefaultTextColor) {
                setTextColor(mSelectTextColor);
            }
            if (!ConvertUtils.isEmpty(mSelectBgColor)) {
                setBackground(mSelectBgColor);
            }
        } else {
            setTextColor(mDefaultTextColor);
            setBackground(mDefaultBgColor);
        }
    }

    /**
     * 跑马一次
     */
    public void startTextViewOneTimeMarquee() {
        setSelected(true);
        setSingleLine();
        setHorizontallyScrolling(true);
        setEllipsize(TextUtils.TruncateAt.MARQUEE);
        setMarqueeRepeatLimit(1);
        setFocusable(true);
        setFocusableInTouchMode(true);
        requestFocus();
    }

    /**
     * 开启跑马灯效果
     */
    public void startTextViewMarquee() {
        Logger.d("lvww", "开启跑马灯效果");
        setSelected(true);
        setSingleLine();
        setHorizontallyScrolling(true);
        setEllipsize(TextUtils.TruncateAt.MARQUEE);
        setMarqueeRepeatLimit(-1);
        setFocusable(true);
        setFocusableInTouchMode(true);
        requestFocus();
    }
}