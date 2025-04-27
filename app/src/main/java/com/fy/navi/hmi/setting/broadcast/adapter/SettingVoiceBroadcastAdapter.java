package com.fy.navi.hmi.setting.broadcast.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemRecommenedVoiceBinding;
import com.fy.navi.service.define.voice.OperationStatus;
import com.fy.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SettingVoiceBroadcastAdapter extends RecyclerView.Adapter<SettingVoiceBroadcastAdapter.Holder>{

    private static final String TAG = "SettingVoiceBroadcastAdapter";

    private List<VoiceInfo> mVoiceInfoList;
    private OnItemClickListener mOnItemClickListener;
    private Context mContext;

    public SettingVoiceBroadcastAdapter(final Context context) {
        this.mContext = context;
    }

    /**
     * 列表刷新数据
     * @param voiceInfoList
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setData(final HashMap<Integer, VoiceInfo> voiceInfoList) {
        mVoiceInfoList = new ArrayList<>(voiceInfoList.values());
        notifyDataSetChanged();
    }

    /**
     * 单选设置
     * @param id
     */
    @SuppressLint("NotifyItemChanged")
    public void setSingleChoice(final int id) {
        int selectIndex = -1;
        int unselectedIndex = -1;
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            if(mVoiceInfoList.get(i).getId() == id){
                selectIndex = i;
                mVoiceInfoList.get(i).setUsed(true);
                continue;
            }
            if(mVoiceInfoList.get(i).isUsed()){
                unselectedIndex = i;
                mVoiceInfoList.get(i).setUsed(false);
            }
        }
        if(selectIndex != -1){
            notifyItemChanged(selectIndex);
        }
        if(unselectedIndex != -1){
            notifyItemChanged(unselectedIndex);
        }
    }

    /**
     * 设置数据列表为Fale
     */
    @SuppressLint("NotifyItemChanged")
    public void unSelectAllVoices() {
        int unselectIndex = -1;
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            if(mVoiceInfoList.get(i).isUsed()){
                unselectIndex = i;
                mVoiceInfoList.get(i).setUsed(false);
                break;
            }
        }
        if(unselectIndex != -1){
            notifyItemChanged(unselectIndex);
        }
    }

    public void updateItem(final int id, final VoiceInfo voiceInfo){
        int updateIndex = -1;
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            if(mVoiceInfoList.get(i).getId() == id){
                updateIndex = i;
                mVoiceInfoList.set(i, voiceInfo);
                break;
            }
        }
        if(updateIndex != -1){
            notifyItemChanged(updateIndex);
        }
    }

    public void setItemClickListener(final OnItemClickListener onItemClickListener) {
        this.mOnItemClickListener = onItemClickListener;
    }

    @NonNull
    @Override
    public SettingVoiceBroadcastAdapter.Holder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final ItemRecommenedVoiceBinding itemRecommenedVoiceBinding = DataBindingUtil.inflate(
                LayoutInflater.from(parent.getContext()), R.layout.item_recommened_voice, parent, false);
        return new Holder(itemRecommenedVoiceBinding);
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    @Override
    public void onBindViewHolder(final @NonNull SettingVoiceBroadcastAdapter.Holder holder, final int position) {
       VoiceInfo voiceInfo = mVoiceInfoList.get(position);
        holder.mVoiceBroadcastBinding.setModel(voiceInfo);





        holder.mVoiceBroadcastBinding.recommendVoiceProgress.setProgressDrawable(
                ResourceUtils.Companion.getInstance().getDrawable(R.drawable.progress_bar_style));

        holder.mVoiceBroadcastBinding.recommendVoiceOperateView.setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                final ArrayList<Integer> operatedIdList = new ArrayList<>();
                operatedIdList.add(voiceInfo.getId());

                switch (voiceInfo.getTaskState()) {
                    case OperationStatus.TASK_STATUS_CODE_READY:
                    case OperationStatus.TASK_STATUS_CODE_PAUSE:
                    case OperationStatus.TASK_STATUS_CODE_ERR:
                    case OperationStatus.TASK_STATUS_CODE_MAX:
                        mOnItemClickListener.startAllTask(operatedIdList);
                        break;
                    case OperationStatus.TASK_STATUS_CODE_CHECKED:
                        ToastUtils.Companion.getInstance().showCustomToastView("验证完成");
                        break;
                    case OperationStatus.TASK_STATUS_CODE_UNZIPPED:
                        ToastUtils.Companion.getInstance().showCustomToastView("解压完成");
                        break;
                    case OperationStatus.TASK_STATUS_CODE_SUCCESS: // 去使用
                        mOnItemClickListener.toUseAllTask(voiceInfo);
                        ToastUtils.Companion.getInstance().showCustomToastView(voiceInfo.getName() +"语音设置成功");
                        break;
                    case OperationStatus.TASK_STATUS_CODE_DOING:
                    case OperationStatus.TASK_STATUS_CODE_DONE:
                    case OperationStatus.TASK_STATUS_CODE_CHECKING:
                    case OperationStatus.TASK_STATUS_CODE_UNZIPPING:
                    case OperationStatus.TASK_STATUS_CODE_WAITING:
                        mOnItemClickListener.pauseAllTask(operatedIdList);
                        break;
                    default:
                        break;
                }

            }
        });
    }

    @Override
    public int getItemCount() {
        if(mVoiceInfoList == null){
            return 0;
        }
        return mVoiceInfoList.size();
    }

    public static class Holder extends RecyclerView.ViewHolder{
        public ItemRecommenedVoiceBinding mVoiceBroadcastBinding;
        public Holder(final ItemRecommenedVoiceBinding voiceBroadcastBinding) {
            super(voiceBroadcastBinding.getRoot());
            this.mVoiceBroadcastBinding = voiceBroadcastBinding;
            voiceBroadcastBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener{
        /**
         * 开始下载
         * @param operatedIdList
         */
        void startAllTask(final ArrayList<Integer> operatedIdList);

        /**
         * 暂停下载
         * @param operatedIdList
         */
        void pauseAllTask(final ArrayList<Integer> operatedIdList);

        /**
         * 去使用
         * @param voiceInfo
         */
        void toUseAllTask(final VoiceInfo voiceInfo);

    }
}
