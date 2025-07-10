package com.sgm.navi.hmi.favorite.adapter;

import android.content.Context;
import android.text.InputType;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.ImageView;
import android.widget.PopupWindow;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.ui.view.SkinEditText;
import com.sgm.navi.ui.view.SkinTextView;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Objects;

/**
 * @author fh
 * @version  $Revision.1$
 */
public class FrequentAddressAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final String TAG = FrequentAddressAdapter.class.getSimpleName();
    private List<PoiInfoEntity> mFrequentAddressList;
    private OnItemClickListener mItemClickListener;
    private static final int TYPE_TEXT = 0;
    private static final int TYPE_IMAGE = 1;
    private final Context mContext;
    private int mSelectedPosition = -1; // -1表示无选中项

    private int mCurrentEditPosition = -1;
    private int mCurrentPosition = -1;
    private WeakReference<SkinEditText> mFocusedEditTextRef;

    private PopupWindow mPopupWindow;
    private TextViewHolder mTextViewHolder;

    public FrequentAddressAdapter(final Context context) {
        this.mContext = context;
    }

    /**
     * setData
     * @param list
     */
    public void setData(final List<PoiInfoEntity> list) {
        mFrequentAddressList = list;
        initPopupWindow();
        notifyDataSetChanged();
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        if (viewType == TYPE_TEXT) {
            final View view = LayoutInflater.from(mContext).inflate(R.layout.item_frequent_address, parent, false);
            return new TextViewHolder(view);
        } else if (viewType == TYPE_IMAGE) {
            final View view = LayoutInflater.from(mContext).inflate(R.layout.item_add_frequent_address, parent, false);
            return new ImageViewHolder(view);
        }
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_add_frequent_address, parent, false);
        return new ImageViewHolder(view);
        //return null; // 这行代码实际上不会被执行，因为上面已经覆盖了所有可能的viewType
    }

    @Override
    public int getItemCount() {
        if (mFrequentAddressList == null) {
            return 1;
        }
        return mFrequentAddressList.size() + 1;// 加1是为了包含最后的图片item
    }

    /**
     * showOrHideSoftInput
     * @param show
     * @param holder
     */
    private void showOrHideSoftInput(final boolean show, final TextViewHolder holder) {
        final InputMethodManager imm = (InputMethodManager) mContext.getSystemService(Context.INPUT_METHOD_SERVICE);
        if (show) {
            imm.showSoftInput(holder.mFrequentAddressTextView, InputMethodManager.SHOW_IMPLICIT);
        } else {
            imm.hideSoftInputFromWindow(holder.mFrequentAddressTextView.getWindowToken(), 0);
        }
    }


    @Override
    public void onBindViewHolder(final @NonNull RecyclerView.ViewHolder holder, final int position) {
        final int itemType = getItemViewType(position);
        if (itemType == TYPE_TEXT) {
            mTextViewHolder = (TextViewHolder) holder;
            final PoiInfoEntity poiInfoEntity = mFrequentAddressList.get(position);
            if (Objects.equals(poiInfoEntity.getFavoriteInfo().getCustom_name(), poiInfoEntity.getName()) ) {
                mTextViewHolder.mFrequentAddressTextView.setText(poiInfoEntity.getFavoriteInfo().getCustom_name());
            } else {
                mTextViewHolder.mFrequentAddressTextView.setText(poiInfoEntity.getName());
            }
            mTextViewHolder.mFrequentAddressTextView.setFocusable(false);
            mTextViewHolder.mFrequentAddressTextView.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE) {
                    mFrequentAddressList.get(position).setName(mTextViewHolder.mFrequentAddressTextView.getText().toString());
                    mTextViewHolder.mFrequentAddressTextView.setFocusable(false);
                    showOrHideSoftInput(false, mTextViewHolder);
                    BehaviorPackage.getInstance().modifyFavoriteData(mFrequentAddressList.get(position).getFavoriteInfo().getItemId(),
                        mTextViewHolder.mFrequentAddressTextView.getText().toString());
                    return true;
                }
                return false;
            });

            mTextViewHolder.mFrequentAddressTextView.setOnClickListener(v -> {
                if (mItemClickListener != null && !mTextViewHolder.mFrequentAddressTextView.isFocused()
                        && !mTextViewHolder.mFrequentAddressTextView.isFocusable()) {
                    mItemClickListener.onItemClick(position);
                }
            });

            mTextViewHolder.mFrequentAddressTextView.setOnFocusChangeListener((v, hasFocus) -> {
                if (hasFocus) {
                    mCurrentEditPosition = position;
                    mFocusedEditTextRef = new WeakReference<>((SkinEditText) v);
                    showOrHideSoftInput(true, mTextViewHolder);
                } else if (mCurrentEditPosition == position){
                    mCurrentEditPosition = -1;
                    mFocusedEditTextRef = null;
                }
            });

            mTextViewHolder.mFrequentAddressMore.setOnClickListener(v -> {
                updatePreviousItem(mTextViewHolder);
                mPopupWindow.dismiss();
                showPopupWindow(mTextViewHolder.mFrequentAddressMore, position);
            });

        } else if (itemType == TYPE_IMAGE) {
            // 不需要设置数据，因为图片是固定的
            holder.itemView.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onItemAddClick(position);
                }
            });
        }
    }


    /**
     * clearPreviousFocus
     */
    public void clearPreviousFocus() {
        // 确保有需要处理的焦点
        if (mCurrentEditPosition != -1 && mFocusedEditTextRef != null) {
            // 通过弱引用获取EditText实例
            final SkinEditText editText = mFocusedEditTextRef.get();

            // 双重验证：视图存在且仍持有焦点
            if (editText != null && editText.hasFocus()) {
                // 清除焦点
                editText.clearFocus();
            }
            // 状态重置
            mCurrentEditPosition = -1;
            mFocusedEditTextRef = null;
        }
    }

    /**
     * initPopupWindow
     */
    private void initPopupWindow() {

        // 创建PopupWindow的视图
        final View popupView = LayoutInflater.from(mContext).inflate(R.layout.favorite_edit_popup, null);

        // 获取PopupWindow中的按钮并设置点击事件
        final SkinTextView btnEdit = popupView.findViewById(R.id.favorite_item_rename);
        final SkinTextView btnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        mPopupWindow = new PopupWindow(popupView, 300, 120,true);
        mPopupWindow.setContentView(popupView);
        mPopupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        btnEdit.setOnClickListener(v -> {
            mPopupWindow.dismiss();

            mTextViewHolder.mFrequentAddressTextView.setImeOptions(EditorInfo.IME_ACTION_DONE);
            mTextViewHolder.mFrequentAddressTextView.setInputType(InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_NORMAL);
            mTextViewHolder.mFrequentAddressTextView.setFocusableInTouchMode(true);
            mTextViewHolder.mFrequentAddressTextView.setFocusable(true);
            mTextViewHolder.mFrequentAddressTextView.requestFocus();
            final String text = mTextViewHolder.mFrequentAddressTextView.getText().toString();
            mTextViewHolder.mFrequentAddressTextView.setSelection(text.length());
        });

        btnDelete.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemDeleteClick(mCurrentPosition);
            }
            mPopupWindow.dismiss();
        });


    }

    /**
     * showPopupWindow
     * @param anchorView
     * @param position
     */
    public void showPopupWindow(final View anchorView ,final int position) {
        // 显示PopupWindow在更多按钮的上方
        mCurrentPosition = position;
        final int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        final int size = mFrequentAddressList.size();
        int x = location[0] - 200;
        int y = location[1] - mPopupWindow.getHeight(); // 计算PopupWindow的Y坐标
        Logger.d(TAG, "showPopupWindow size = " + size + ",currentPosition = " + position);
        switch (size) {
            case 2 :
                if (position == 0) {
                    x = x - 590;
                }
                break;
            case 3 :
                if (position == 0) {
                    y = y - 150;
                } else if (position == 1) {
                    x = x + 590;
                    y = y - 150;
                }
                break;
            default:
                break;
        }

        mPopupWindow.showAtLocation(anchorView, Gravity.NO_GRAVITY, x, y);
    }


    /**
     * updatePreviousItem
     * @param holder
     */
    private void updatePreviousItem(final TextViewHolder holder) {

        // 先清除前一个焦点
        clearPreviousFocus();

        final int previousSelected = mSelectedPosition;
        mSelectedPosition = holder.getAdapterPosition();

        // 取消之前选中的Item
        if (previousSelected != -1 && previousSelected != holder.getAdapterPosition()) {
            notifyItemChanged(previousSelected);
        }
    }

    @Override
    public int getItemViewType(final int position) {
        if (position == getItemCount() - 1) {
            return TYPE_IMAGE;
        } else {
            return TYPE_TEXT;
        }
    }

    public static class TextViewHolder extends RecyclerView.ViewHolder {
        private SkinEditText mFrequentAddressTextView;
        private ImageView mFrequentAddressMore;

        public TextViewHolder(final @NonNull View itemView) {
            super(itemView);
            mFrequentAddressTextView = itemView.findViewById(R.id.tv_frequent_address_title);
            mFrequentAddressMore= itemView.findViewById(R.id.tv_frequent_address_more);
        }
    }

    public static class ImageViewHolder extends RecyclerView.ViewHolder {
        private ImageView mAddView;

        public ImageViewHolder(final @NonNull View itemView) {
            super(itemView);
            mAddView = itemView.findViewById(R.id.iv_add1);
        }
    }

    public interface OnItemClickListener {
        /**
         * onItemClick
         * @param index
         */
        void onItemClick(int index);

        /**
         * onItemDeleteClick
         * @param index
         */
        void onItemDeleteClick(int index);

        /**
         * onItemAddClick
         * @param index
         */
        void onItemAddClick(int index);
    }
}
