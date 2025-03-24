package com.fy.navi.scene.impl.navi.common;


import android.content.Context;
import android.widget.TextView;

import androidx.annotation.StringRes;

/**
 * 统一的文案接收入口
 * 可用于承载多语言翻译、其他特殊的文案处理述求。
 * @author fy
 * @version $Revision.*$
 */
public class AutoUIString {
    private String mStr;
    private CharSequence mCharSequence;
    private @StringRes int mStrId = 0;
    private Object[] mFormatArgs;

    public AutoUIString(final String str) {
        this.mStr = str;
    }

    public AutoUIString(@StringRes final int strId) {
        this.mStrId = strId;
    }

    public AutoUIString(final CharSequence charSequence) {
        this.mCharSequence = charSequence;
    }

    public AutoUIString(@StringRes final int strId, final Object... formatArgs) {
        this.mStrId = strId;
        this.mFormatArgs = formatArgs;
    }

    /**
     * 获取字符串
     * @param context 上下文
     * @return String
     */
    public String getString(final Context context) {
        final CharSequence charSequence = getCharSequence(context);
        if (null == charSequence) {
            return null;
        }
        return charSequence.toString();
    }

    /**
     * 获取字符序列
     * @param context 上下文
     * @return CharSequence
     */
    public CharSequence getCharSequence(final Context context) {
        if (mStr != null) {
            return mStr;
        }
        if (mCharSequence != null) {
            return mCharSequence;
        }
        if (mFormatArgs != null && mStrId != 0) {
            return context.getString(mStrId, mFormatArgs);
        }

        if (mStrId != 0) {
            return context.getString(mStrId);
        }
        return null;
    }

    /**
     * 设置文本
     * @param textView textView
     */
    public void setText(final TextView textView) {
        if (mStrId != 0) {
            if (mFormatArgs != null) {
                textView.setText(textView.getContext().getString(mStrId, mFormatArgs));
            } else {
                textView.setText(mStrId);
            }
        } else if (mStr != null) {
            textView.setText(mStr);
        }
    }

    /**
     * 设置提示
     * @param textView textView
     */
    public void setHint(final TextView textView) {
        if (mStrId != 0) {
            if (mFormatArgs != null) {
                textView.setHint(textView.getContext().getString(mStrId, mFormatArgs));
            } else {
                textView.setHint(mStrId);
            }
        } else if (mStr != null) {
            textView.setHint(mStr);
        }
    }

    /**
     * 解析AutoUIString获取最后的字符串
     */
    private static class AutoGetStr {
        private String mStr;
        private @StringRes int mStrId = 0;

        public AutoGetStr(final AutoUIString autoUIString) {
            // 将来如果需要动态实时翻译时，formatArgs里面的字符串也应该使用AutoUIString来表示
        }
    }

}
