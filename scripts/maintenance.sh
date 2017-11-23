#!/bin/bash
echo ""
echo "####################################################"
echo "## This is a script to perform folder maintenance ##"
echo "####################################################"
echo ""


DRY=false
FULL=false
while getopts ":nf" opt; do
    case $opt in
        n)
            echo "Dry run (-n) was triggered!" >&2
            DRY=true
            ;;
        f)
            echo "I will synchronize the full stuff." >&2
            FULL=true
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
    esac
done
shift $((OPTIND-1))


if [ "$DRY" = true ]; then

    rsync -avumn $VSC_SCRATCH_VO/manfredo/*.sh $VSC_DATA_VO/manfredo/scripts
    if [ "$FULL" = true ]; then
        rsync -avumn $VSC_SCRATCH/* $VSC_DATA/
        rsync -avumn --exclude "*.sh" $VSC_SCRATCH/manfredo/* $VSC_DATA/manfredo/
    fi

else

    rsync -avum $VSC_SCRATCH_VO/manfredo/*.sh $VSC_DATA_VO/manfredo/scripts
    if [ "$FULL" = true ]; then
        rsync -avum $VSC_SCRATCH/* $VSC_DATA/
        rsync -avum --exclude "*.sh" $VSC_SCRATCH/manfredo/* $VSC_DATA/manfredo/
    fi
fi

